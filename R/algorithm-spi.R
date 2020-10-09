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

#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{grid_terrain} or \link{normalize_height}. It implements an algorithm
#' for spatial interpolation. Spatial interpolation is based on a Delaunay triangulation, which performs
#' a linear interpolation within each triangle. There are usually a few points outside the convex hull,
#' determined by the ground points at the very edge of the dataset, that cannot be interpolated with
#' a triangulation. Extrapolation is done using the nearest neighbour approach.
#'
#' @param ... unused
#' @param extrapolate There are usually a few points outside the convex hull, determined by the ground
#' points at the very edge of the dataset, that cannot be interpolated with a triangulation.
#' Extrapolation is done using the nearest neighbour approach by default using \link{knnidw}.
#'
#' @export
#'
#' @family spatial interpolation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # plot(las)
#'
#' dtm = grid_terrain(las, algorithm = tin())
#'
#' plot(dtm, col = terrain.colors(50))
#' plot_dtm3d(dtm)
tin = function(..., extrapolate = knnidw(3,1,50))
{
  if (!is.null(extrapolate))
    assert_is_algorithm_spi(extrapolate)

  extrapolate <- lazyeval::uq(extrapolate)

  f = function(what, where, scales = c(0,0), offsets = c(0,0))
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "tin")
    z <- interpolate_delaunay(what, where, trim = 0, scales = scales, offsets = offsets, min_normal_z = 3e-2)

    # Extrapolate beyond the convex hull
    isna <- is.na(z)
    nnas <- sum(isna)
    if (nnas > 0 && !is.null(extrapolate))
    {
      verbose("Interpolating the points ouside the convex hull of the ground points using knnidw()")

      lidR.context <- "spatial_interpolation"
      what <- data.frame(X = where$X[!isna], Y = where$Y[!isna], Z = z[!isna])
      where <- data.frame(X = where$X[isna],  Y = where$Y[isna])
      zknn <- extrapolate(what, where, scales, offsets)
      z[isna] <- zknn
      isna <- is.na(zknn)
      nnas <- sum(isna)

      if (nnas > 0)
        message(glue::glue("Interpolation of {nnas} points outside the convex hull defined by ground points (outside the triangulation) failed and returned NAs."))
    }

    return(z)
  }

  class(f) <- c(LIDRALGORITHMSPI, LIDRALGORITHMOPENMP)
  return(f)
}

#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{grid_terrain} or \link{normalize_height}. It implements an algorithm
#' for spatial interpolation. Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse-distance weighting (IDW).
#'
#' @param k integer. Number of k-nearest neighbours. Default 10.
#' @param p numeric. Power for inverse-distance weighting. Default 2.
#' @param rmax numeric. Maximum radius where to search for knn. Default 50.
#'
#' @export
#'
#' @family spatial interpolation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # plot(las)
#'
#' dtm = grid_terrain(las, algorithm = knnidw(k = 6L, p = 2))
#'
#' plot(dtm, col = terrain.colors(50))
#' plot_dtm3d(dtm)
knnidw = function(k = 10, p = 2, rmax = 50)
{
  k <- lazyeval::uq(k)
  p <- lazyeval::uq(p)
  rmax <- lazyeval::uq(rmax)

  f = function(what, where, scales = c(0,0), offsets = c(0,0))
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "knnidw")
    return(interpolate_knnidw(what, where, k, p, rmax))
  }

  class(f) <- c(LIDRALGORITHMSPI, LIDRALGORITHMOPENMP)
  return(f)
}

#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{grid_terrain} or \link{classify_ground}. It implements an algorithm
#' for spatial interpolation. Spatial interpolation is based on universal kriging using the \link[gstat:krige]{krige}
#' function from \code{gstat}. This method combines the KNN approach with the kriging approach. For each
#' point of interest it kriges the terrain using the k-nearest neighbour ground points. This method
#' is more difficult to manipulate but it is also the most advanced method for interpolating spatial data.
#'
#' @param k numeric. Number of k-nearest neighbours. Default 10.
#'
#' @param model A variogram model computed with \link[gstat:vgm]{vgm}. If NULL it performs an ordinary
#' or weighted least squares prediction.
#'
#' @export
#'
#' @family spatial interpolation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # plot(las)
#'
#' dtm = grid_terrain(las, algorithm = kriging())
#'
#' plot(dtm, col = terrain.colors(50))
#' plot_dtm3d(dtm)
kriging = function(model = gstat::vgm(.59, "Sph", 874), k = 10L)
{
  f = function(what, where, scales = c(0,0), offsets = c(0,0))
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "kriging")
    return(interpolate_kriging(what, where, model, k))
  }

  class(f) <- LIDRALGORITHMSPI
  return(f)
}

interpolate_knnidw = function(points, coord, k, p, rmax = 50)
{
  z <- C_knnidw(points$X, points$Y, points$Z, coord$X, coord$Y, k, p, rmax, getThread())
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

interpolate_delaunay <- function(points, coord, trim = 0, scales = c(1,1), offsets = c(0,0), options = "QbB", min_normal_z = 0)
{
  stopifnot(is.numeric(trim), length(trim) == 1L)
  stopifnot(is.numeric(scales), length(scales) == 2L)
  stopifnot(is.numeric(offsets), length(offsets) == 2L)
  stopifnot(is.data.frame(coord))

  boosted_triangulation <- TRUE

  if (inherits(points, "LAS")) {
    xscale  <- points@header@PHB[["X scale factor"]]
    yscale  <- points@header@PHB[["Y scale factor"]]
    xoffset <- points@header@PHB[["X offset"]]
    yoffset <- points@header@PHB[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)
    points  <- points@data
  }

  stopifnot(is.data.frame(points))

  if (scales[1] != scales[2]) {
    message("The Delaunay triangulation reverted to the old slow method because xy scale factors are different so the fast method cannot be applied.")
    boosted_triangulation <- FALSE
  }

  # Check if coordinates actually match the resolution
  # Check only 100 of them including the first one
  n <- min(100, length(points$X)) - 1
  s <- c(1, sample(2:length(points$X), n))
  X <- points$X[s]
  Y <- points$Y[s]
  x <- fast_countunquantized(X, scales[1], offsets[1])
  y <- fast_countunquantized(Y, scales[2], offsets[2])

  if (x > 0 | y > 0)
  {
    message("The Delaunay triangulation reverted to the old slow method because xy coordinates were not convertible to integer values. xy scale factors and offsets are likely to be invalid")
    boosted_triangulation <- FALSE
  }

  if (boosted_triangulation) {
    return(C_interpolate_delaunay(points, coord, scales, offsets, trim, min_normal_z, getThreads()))
  }
  else {
    P <- as.matrix(points)
    X <- as.matrix(coord)
    D <- tDelaunay(P, trim = trim)
    return(tInterpolate(D, P, X, getThreads()))
  }
}
