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

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{grid_canopy}. It implements an algorithms for digital
#' surface model computation based on a points-to-raster method: for each pixel of the output raster
#' the function attributes the height of the highest point found. The \code{subcircle} tweak replaces
#' each point with 8 points around the original one. This allows for virtual 'emulation' of the fact
#' that a lidar point is not a point as such, but more realistically a disc. This tweak densifies the
#' point cloud and the resulting canopy model is smoother and contains fewer 'pits' and empty pixels.
#'
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Points-to-raster algorithm with a resolution of 1 meters
#' chm <- grid_canopy(las, res = 1, p2r())
#' plot(chm, col = col)
#'
#' # Points-to-raster algorithm with a resolution of 0.5 meter replacing each
#' # point by a 20 cm radius circle of 8 points
#' chm <- grid_canopy(las, res = 0.5, p2r(0.2))
#' plot(chm, col = col)
p2r = function(subcircle = 0)
{
  assertive::assert_is_a_number(subcircle)
  assertive::assert_all_are_non_negative(subcircle)

  f = function(las, layout)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("grid_canopy"), "p2r")

    bbox <- raster::extent(layout)
    dsm  <- C_grid_canopy(las, raster::as.matrix(bbox), raster::res(layout)[1], subcircle)
    dsm  <- t(dsm)

    if (!all(dim(layout)[1:2] == dim(dsm)))
      stop("Internal error: matrix returned at the C++ level don't match with the layout. Please report this bug.", call. = FALSE)

    return(dsm)
  }

  class(f) <- c("function", "DigitalSurfaceModel", "Algorithm", "lidR")
  return(f)
}

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{grid_canopy}. It implements an algorithms for digital
#' surface model computation using a Delaunay triangulation of first returns with a linear interpolation
#' within each triangle.
#'
#' @param max_edge numeric. Maximum edge-length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge greater than this value it will be removed to trim dummy interpolation
#' on non-convex areas. If \code{max_edge = 0} no trimming is done (see examples).
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- grid_canopy(las, res = 1, dsmtin())
#' plot(chm, col = col)
#'
#' \dontrun{
#' # Potentially complex concave subset of point cloud
#' x = c(481340, 481340, 481280, 481300, 481280, 481340)
#' y = c(3812940, 3813000, 3813000, 3812960, 3812940, 3812940)
#' las2 = lasclipPolygon(las,x,y)
#' plot(las2)
#'
#' # The TIN interpolation being done within the convex hull of the point cloud there
#' # are lot of dummy pixels that are strictly correct regarding the interpolation method
#' used but meaningless in our CHM
#' chm <- grid_canopy(las2, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Use 'max_edge' to trim dummy triangles
#' chm = grid_canopy(las2, res = 0.5, dsmtin(max_edge = 3))
#' plot(chm, col = col)
#' }
dsmtin = function(max_edge = 0)
{
  return(pitfree(0, c(max_edge, 0), 0))
}

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{grid_canopy}. It implements the pit-free algorithm
#' developed by Khosravipour et al. (2014), which is based on the computation of a set of classical
#' triangulations at different heights (see references). The \code{subcircle} tweak replaces each
#' point with 8 points around the original one. This allows for virtual 'emulation' of the fact that
#' a lidar point is not a point as such, but more realistically a disc. This tweak densifies the point
#' cloud and the resulting canopy model is smoother and contains fewer 'pits' and empty pixels.
#'
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#'
#' @param thresholds numeric. Set of height thresholds accoring to Khosravipour et al. (2014) algorithm
#' description (see references)
#'
#' @param max_edge numeric. Maximum edge-length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge greater than this value it will be removed. The first number is the value
#' for the classical triangulation (threshold = 0, see also \link{dsmtin}), the second number
#' is the value for the pit-free algorithm (for thresholds > 0). If \code{max_edge = 0} no trimming
#' is done (see examples).
#'
#' @references Khosravipour, A., Skidmore, A. K., Isenburg, M., Wang, T., & Hussin, Y. A. (2014).
#' Generating pit-free canopy height models from airborne lidar. Photogrammetric Engineering &
#' Remote Sensing, 80(9), 863-872.
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- grid_canopy(las, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm <- grid_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
#' plot(chm, col = col)
#'
#' \dontrun{
#' # Potentially complex concave subset of point cloud
#' x = c(481340, 481340, 481280, 481300, 481280, 481340)
#' y = c(3812940, 3813000, 3813000, 3812960, 3812940, 3812940)
#' las2 = lasclipPolygon(las,x,y)
#' plot(las2)
#'
#' # The TIN interpolation being done within the convex hull of the point cloud there are lot of
#' # dummy pixels that are strictly correct regarding the interpolation method used but meaningless
#' # in our CHM
#' chm <- grid_canopy(las2, res = 0.5, pitfree())
#' plot(chm, col = col)
#'
#' chm = grid_canopy(las2, res = 0.5, pitfree(max_edge = c(3, 1.5)))
#' plot(chm, col = col)
#' }
#' @export
pitfree = function(thresholds = c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0)
{
  assertive::assert_is_numeric(thresholds)
  assertive::assert_all_are_non_negative(thresholds)
  assertive::assert_is_numeric(max_edge)
  assertive::assert_all_are_non_negative(max_edge)
  assertive::assert_is_a_number(subcircle)
  assertive::assert_all_are_non_negative(subcircle)

  if (length(thresholds) > 1L & length(max_edge) < 2L)
    stop("'max_edge' should contain 2 numbers", call. = FALSE)

  f = function(las, layout)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("grid_canopy"), "pitfree")

    if (!"ReturnNumber" %in% names(las@data))
      stop("No attribute 'ReturnNumber' found. This attribute is needed to extract first returns", call. = FALSE)

    if (fast_countequal(las@data$ReturnNumber, 1L) == 0)
      stop("No first returns found. Operation aborted.", call. = FALSE)

    . <- X <- Y <- Z <- ReturnNumber <- NULL

    # Initialize the interpolated values with NAs
    z = rep(NA_real_, raster::ncell(layout))

    # Get only first returns and coordinates (nothing else needed)
    verbose("Selecting first returns...")

    cloud = las@data
    if (fast_countequal(las@data$ReturnNumber, 1) < nrow(las@data))
      cloud = las@data[ReturnNumber == 1, .(X,Y,Z)]

    # subcircle the data
    if (subcircle > 0)
    {
      verbose("Subcircling points...")

      ex = raster::extent(las)
      cloud = subcircled(cloud, subcircle, 8)
      cloud = cloud[between(X, ex@xmin, ex@xmax) & between(Y, ex@ymin, ex@ymax)]
    }

    verbose("Selecting only the highest points within the grid cells...")

    cells = raster::cellFromXY(layout, cloud[, .(X,Y)])
    grid  = raster::xyFromCell(layout, 1:raster::ncell(layout))
    grid = data.table::as.data.table(grid)
    data.table::setnames(grid, c("x", "y"), c("X", "Y"))
    cloud = cloud[cloud[, .I[which.max(Z)], by = cells]$V1]

    # Perform the triangulation and the rasterization (1 loop for classical triangulation, several for Khosravipour et al.)
    i = 1
    for (th in thresholds)
    {
      verbose(glue::glue("Triangulation pass {i} of {length(thresholds)}..."))
      i =  i+ 1

      if (th == 0)
        edge = max_edge[1]
      else
        edge = max_edge[2]

      cloud = cloud[Z >= th]

      if (nrow(cloud) > 0)
      {
        Ztemp = interpolate_delaunay(cloud, grid, edge)
        z = pmax(z, Ztemp, na.rm = T)
      }
    }

    if(all(is.na(z)))
      stop("Interpolation failed. Input parameters might be wrong.", call. = FALSE)

    return(z)
  }

  class(f) <- c("function", "DigitalSurfaceModel", "Algorithm", "lidR")
  return(f)
}