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

interpolate = function(points, coord, method, k, model)
{
  . <- X <- Y <- Z <- NULL

  if(dim(coord)[1] == 0)
    return(numeric(0))

  # test integrity of the data
  dup_xyz  = duplicated(points, by = c("X", "Y", "Z"))
  dup_xy   = duplicated(points, by = c("X", "Y"))
  ndup_xyz = sum(dup_xyz)
  ndup_xy  = sum(dup_xy & !dup_xyz)

  if(ndup_xyz > 0)
    warning(paste("There were",  ndup_xyz, "ground points with duplicated X Y Z coordinates. They were removed."), call. = FALSE)

  if(ndup_xy > 0)
    warning(paste("There were", ndup_xy, "duplicated ground points. Some X Y coordinates were repeated but with different Z coordinates. min Z were retained."), call. = FALSE)

  if(ndup_xy > 0 | ndup_xyz > 0)
    points = points[, .(Z = min(Z)), by = .(X,Y)]

  if(method == "knnidw")
  {
    cat("[using inverse distance weighting]\n")
    return(interpolate_knnidw(points, coord, k))
  }
  else if(method == "delaunay")
  {
    cat("[using Delaunay triangulation]\n")
    return(interpolate_delaunay(points, coord, 0))
  }
  else if(method == "kriging")
  {
    return(interpolate_kriging(points, coord, model, k))
  }
  else
    stop(paste0("Method '", method, "' does not exist."), call. = FALSE)
}

interpolate_knnidw = function(points, coord, k)
{
  . <- X <- Y <- NULL

  nn = RANN::nn2(points[, .(X,Y)], coord[, .(X,Y)], k = k)
  idx = nn$nn.idx
  w = 1/nn$nn.dist
  w = ifelse(is.infinite(w), 1e8, w)
  z = matrix(points[as.numeric(idx)]$Z, ncol = dim(w)[2])

  return(rowSums(z*w)/rowSums(w))
}

interpolate_kriging = function(points, coord, model, k)
{
  X <- Y <- Z <- NULL
  x  = gstat::krige(Z~X+Y, location = ~X+Y, data = points, newdata = coord, model, nmax = k)
  return(x$var1.pred)
}

interpolate_delaunay <- function(points, coord, th)
{
  pitfree = th > 0

  X <- as.matrix(points)
  Y <- as.matrix(coord)

  dn   <- suppressMessages(geometry::delaunayn(X[,1:2]))
  idx  <- geometry::tsearch(X[,1],X[,2],dn,Y[,1],Y[,2])
  uidx <- unique(idx)
  uidx <- uidx[!is.na(uidx)]

  active <- dn[uidx,]
  active <- cbind(active, uidx)

  N = get_normales(active, X, nrow(dn), pitfree)
  N = N[idx,]

  z = -(Y[,1]*N[,1] + Y[,2]*N[,2]+N[,4])/N[,3]

  if(pitfree)
  {
    delete = N[,5] > th
    delete[is.na(delete)] = FALSE
    z[delete] = NA
  }

  return(z)
}