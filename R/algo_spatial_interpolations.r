# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' Algorithms for spatial interpolation
#'
#' These functions are made to be used in \link{lasnormalize} or \link{grid_terrain} (see details).
#' They implement spatial interpolation algorithms.\cr
#' \describe{
#' \item{\code{knnidw}}{Spatial interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse distance weighting (IDW). This is a fast but basic method for spatial data interpolation.}
#' \item{\code{tin}}{Spatial interpolation is based on a Delaunay triangulation. It performs a linear
#' interpolation within each triangle. There are usually a few points outside the convex hull,
#' determined by the ground points at the very edge of the dataset, which cannot be interpolated
#' with a triangulation. Extrapolation is done using the nearest neighbour.}
#' \item{\code{kriging}}{Spatial interpolation based on universal kriging using the \link[gstat:krige]{krige}
#' function from gstat. This method combines the KNN approach with the kriging approach. For each
#' point of interest it kriges the terrain using the k-nearest neighbour ground points. This method
#' is more difficult to manipulate but it is also the most advanced method for interpolating spatial data. }
#' }
#'
#' @param k numeric. Number of k-nearest neighbours. Default 10.
#' @param p numeric. Power for inverse distance weighting. Default 2.
#' @param model A variogram model computed with \link[gstat:vgm]{vgm}. If NULL it performs an ordinary
#' or weighted least squares prediction.
#'
#' @aliases SpatialInterpolationFunctions
#' @rdname SpatialInterpolationFunctions
#' @export
#' @family Algorithm
#' @family Spatial Interpolation
tin = function()
{
  f = function(what, where)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasnormalize", "grid_terrain", "grid_canopy"), "tin")

    z    <- interpolate_delaunay(what, where)
    isna <- is.na(z)
    nnas <- sum(isna)

    if (nnas > 0)
    {
      z[isna] <- C_knnidw(where$X[!isna], where$Y[!isna], z[!isna], where$X[isna], where$Y[isna], 1, 1)
      message(glue::glue("{nnas} points outside the convex hull of the triangulation were interpolated using the nearest neighbour."))
    }

    return(z)
  }

  class(f) <- c("SpatialInterpolation", "Algorithm", "lidR")
  return(f)
}

#' @rdname SpatialInterpolationFunctions
#' @export
knnidw = function(k = 10, p = 2)
{
  f = function(what, where)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasnormalize", "grid_terrain", "grid_canopy"), "knnidw")

    z <- interpolate_knnidw(what, where, k, p)
    return(z)
  }

  class(f) <- c("SpatialInterpolation", "Algorithm", "lidR")
  return(f)
}

#' @rdname SpatialInterpolationFunctions
#' @export
kriging = function(model = gstat::vgm(.59, "Sph", 874), k = 10L)
{
  f = function(what, where)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasnormalize", "grid_terrain", "grid_canopy"), "kriging")

    z <- interpolate_kriging(what, where, model, k)
    return(z)
  }

  class(f) <- c("SpatialInterpolation", "Algorithm", "lidR")
  return(f)
}

interpolate_knnidw = function(points, coord, k, p)
{
  z <- C_knnidw(points$X, points$Y, points$Z, coord$X, coord$Y, k, p)
  return(z)
}

interpolate_kriging = function(points, coord, model, k)
{
  X <- Y <- Z <- NULL

  if (!getOption("lidR.verbose"))
    sink(tempfile())

  x  <- gstat::krige(Z~X+Y, location = ~X+Y, data = points, newdata = coord, model, nmax = k)

  sink()

  return(x$var1.pred)
}

interpolate_delaunay <- function(points, coord, th = 0)
{
  pitfree <- th > 0  # specific case if using khosravipour algorithm in grid_tincanopy

  verbose("Delaunay triangulation...")

  X <- as.matrix(points)
  Y <- as.matrix(coord)

  dn   <- suppressMessages(geometry::delaunayn(X[,1:2], options = "QbB"))

  # geometry::trimesh(dn, X, asp = 1)

  verbose("Searching for the enclosing Delaunay convex hull...")

  idx  <- C_tsearch(points$X, points$Y, dn, coord$X, coord$Y)

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