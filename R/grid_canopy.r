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



#' Canopy surface model
#'
#' Creates a canopy surface model using several possible algorithm.
#'
#' \describe{
#' \item{p2r}{points-to-raster based method: for each pixel of the ouput raster the function attribute
#' the height of the highest point found.}
#' \item{tin}{triangulation based method: Delaunay triangulation of first returns with a linear
#' interpolation within each triangle.}
#' \item{pitfree}{the pit-free algorithm developed by Khosravipour et al. (2014), which is based on
#' the computation of a set of classical triangulations at different heights (see references).}
#' }
#' \cr The \code{subcircle} tweak replaces each point with 8 points around the original one. This allows
#' for virtual 'emulation' of the fact that a lidar point is not a point as such, but more
#' realistically a disc. This tweak densifies the point cloud and the resulting canopy model is
#' smoother and contains fewer 'pits' and empty pixels.
#'
#' @template LAScatalog
#'
#' @template param-las
#' @param algorithm character. The name of an algorithm. Can be \code{"p2r"},
#' \code{"tin"} or \code{"pitfree"}. (see details)
#' @param ... parameters for the algorithms. These depend on the algorithm used (see documentation
#' of each method).
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units.
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#' @param thresholds numeric. Set of height thresholds accoring to Khosravipour et al. (2014) algorithm
#' description (see references)
#' @param max_edge numeric. Maximum edge-length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge greater than this value it will be removed. It is used to drive
#' the pit-free algorithm and to trim dummy interpolation on non-convex areas.
#' The first number is the value for the classical triangulation (threshold = 0), the second number
#' is the value for the pit-free algorithm (for thresholds > 0). If \code{max_edge = 0} no trimming
#' is done (see examples.

#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Points-to-raster algorithm with a resolution of 1 meters
#' chm <- grid_canopy(las, "p2r", res = 1)
#' plot(chm, col = col)
#'
#' # Points-to-raster algorithm with a resolution of 0.5 meter replacing each
#' # point by a 20 cm radius circle of 8 points
#' chm <- grid_canopy(las, "p2r", res = 0.5, subcircle = 0.2)
#' plot(chm, col = col)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- grid_canopy(las, "tin", res = 0.5)
#' plot(chm, col = col)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm <- grid_canopy(las, "pitfree", res = 0.5, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
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
#' chm <- grid_canopy(las2, "tin", res = 0.5)
#' plot(chm, col = col)
#'
#' # Use 'max_edge' to trim dummy triangles
#' chm = grid_canopy(las2, "tin", res = 0.5, max_edge = 3)
#' plot(chm, col = col)
#'
#' chm = grid_canopy(las2, "pitfree", res = 0.5, max_edge = c(3, 1.5))
#' plot(chm, col = col)
#' }
#' @references Khosravipour, A., Skidmore, A. K., Isenburg, M., Wang, T., & Hussin, Y. A. (2014).
#' Generating pit-free canopy height models from airborne lidar. Photogrammetric Engineering &
#' Remote Sensing, 80(9), 863-872.
#' @export
grid_canopy = function(las, algorithm, ...)
{
  if (algorithm == "p2r")
   return(grid_canopy_p2r(las, ...))
  else if (algorithm == "tin")
    return(grid_canopy_tin(las, ...))
  else if (algorithm == "pitfree")
    return(grid_canopy_pitfree(las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}
