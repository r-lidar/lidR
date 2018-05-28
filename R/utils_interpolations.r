# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================

interpolate = function(points, coord, method, k, p, model, wbuffer = TRUE)
{
  . <- X <- Y <- Z <- NULL

  if (dim(coord)[1] == 0)
    return(numeric(0))

  # test integrity of the data and degenerated points
  dup_xyz  = duplicated(points, by = c("X", "Y", "Z"))
  dup_xy   = duplicated(points, by = c("X", "Y"))
  ndup_xyz = sum(dup_xyz)
  ndup_xy  = sum(dup_xy & !dup_xyz)

  if (ndup_xyz > 0)
    warning(glue("There were {ndup_xyz} degenerated ground points. Some X Y Z coordinates were repeated. They were removed."), call. = FALSE)

  if (ndup_xy > 0)
    warning(glue("There were {ndup_xy} degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates. min Z were retained."), call. = FALSE)

  if (ndup_xy > 0 | ndup_xyz > 0)
    points = points[, .(Z = min(Z)), by = .(X,Y)]

  if (method == "knnidw")
  {
    verbose("[using inverse distance weighting]")
    return(interpolate_knnidw(points, coord, k, p))
  }
  else if (method == "delaunay")
  {
    verbose("[using Delaunay triangulation]")

    z = interpolate_delaunay(points, coord)

    isna = is.na(z)
    nnas = sum(isna)

    if (nnas > 0 & k > 0)
    {
      z[isna] <- C_knnidw(coord$X[!isna], coord$Y[!isna], z[!isna], coord$X[isna], coord$Y[isna], 1, 1)

      if(wbuffer)
        message(glue("{nnas} points outside the convex hull of the triangulation were interpolated using the nearest neighbour."))
    }

    return(z)
  }
  else if (method == "kriging")
  {
    return(interpolate_kriging(points, coord, model, k))
  }
  else
    stop(glue("Method '{method}' does not exist."), call. = FALSE)
}

interpolate_knnidw = function(points, coord, k, p)
{
  z = C_knnidw(points$X, points$Y, points$Z, coord$X, coord$Y, k, p)
  return(z)
}

interpolate_kriging = function(points, coord, model, k)
{
  X <- Y <- Z <- NULL

  if (!LIDROPTIONS("verbose"))
    sink(tempfile())

  x  = gstat::krige(Z~X+Y, location = ~X+Y, data = points, newdata = coord, model, nmax = k)

  sink()

  return(x$var1.pred)
}

interpolate_delaunay <- function(points, coord, th = 0)
{
  pitfree = th > 0  # specific case if using khosravipour algorithm in grid_tincanopy

  verbose("Delaunay triangulation...")

  X <- as.matrix(points)
  Y <- as.matrix(coord)

  dn   <- suppressMessages(geometry::delaunayn(X[,1:2], options = "QbB"))

  # geometry::trimesh(dn, X, asp = 1)

  verbose("Searching for the enclosing Delaunay convex hull...")

  idx  <- C_tsearch(points$X, points$Y, dn, coord$X, coord$Y, LIDROPTIONS("progress"))

  #uidx <- unique(idx)
  #uidx <- uidx[!is.na(uidx)]

  #active <- dn[uidx,]
  #active <- cbind(active, uidx)

  verbose("Rasterizing the triangulation...")

  N = C_tinfo(dn, X)
  N = N[idx,]

  z = -(Y[,1] * N[,1] + Y[,2] * N[,2] + N[,4]) / N[,3]

  if (pitfree)
  {
    verbose("Removing triangles larger than threshold...")

    delete = N[,7] > th
    delete[is.na(delete)] = FALSE
    z[delete] = NA
  }

  return(z)
}