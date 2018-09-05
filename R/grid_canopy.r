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

#' Digital Surface Model
#'
#' Creates a digital surface model using several possible algorithms.
#'
#' @template param-las
#' @param algorithm function. A function that implements an algorithm to compute a canopy height model. lidR have
#' three of these function: \link{p2r}, \link{dsmtin}, \link{pitfree} (see respective documentations
#' and exemples).
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units.
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
#' chm <- grid_canopy(las2, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Use 'max_edge' to trim dummy triangles
#' chm = grid_canopy(las2, res = 0.5, dsmtin(max_edge = 3))
#' plot(chm, col = col)
#'
#' chm = grid_canopy(las2, res = 0.5, pitfree(max_edge = c(3, 1.5)))
#' plot(chm, col = col)
#' }
#' @export
grid_canopy = function(las, res, algorithm)
{
  UseMethod("grid_canopy", las)
}

#' @export
grid_canopy.LAS = function(las, res, algorithm)
{

  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "DigitalSurfaceModel"))
    stop("The algorithm is not an algorithm for digital surface model.", call. = FALSE)

  . <- X <- Y <- Z <- NULL

  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)

  subcircle <- as.list(environment(algorithm))$subcircle
  subcircle <- if(is.null(subcircle)) 0 else subcircle

  layout <- make_overlay_raster(las, res, subcircle = subcircle)
  names(layout) <- "Z"

  lidR.context <- "grid_canopy"
  z = algorithm(las, layout)

  suppressWarnings(layout[] <- z)
  return(layout)
}

#' @export
grid_canopy.LAScluster = function(las, res, algorithm)
{
  x = readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox = raster::extent(las)
  metrics = grid_canopy(x, res, algorithm)
  metrics = raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
grid_canopy.LAScatalog = function(las, res, algorithm)
{
  set_select(las) <- "xyzr"
  output <- catalog_apply2(las, grid_canopy, res = res, algorithm = algorithm, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (get_output_files(las) != "")  return(unlist(output))

  # Outputs have been returned in R objects. Merge the outputs in a single object
  return(merge_rasters(output))
}