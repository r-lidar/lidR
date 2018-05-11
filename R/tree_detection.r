# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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

#' Tree top detection based on local maxima filters
#'
#' Tree top detection based on local maxima filters. There are two types of filter. The
#' first, called for gridded objects, works on images with a matrix-based algorithm
#' and the second one, called for point clouds, works at the point cloud level without any
#' rasterization.
#'
#' @param x A object of class \code{LAS} or an object representing a canopy height model
#' such as a \code{RasterLayer} or a \code{lasmetrics} or a \code{matrix}.
#' @param ws numeric. Size of the moving window used to the detect the local maxima. On
#' a raster-like object this size is in pixels and should be an odd number larger than 3.
#' On a raw point cloud this size is in the point cloud units (usually meters).
#' @param hmin numeric. Minimum height of a tree. Threshold below which a pixel or a point
#' cannot be a local maxima. Default 2.
#'
#' @return A \code{data.table} with the coordinates of the tree tops (X, Y, Z) if the input
#' is a point cloud, or a RasterLayer if the input is a raster-like object.
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # point-cloud-based method
#'
#' ttops = tree_detection(las, 5)
#'
#' plot(las)
#' with(ttops, rgl::points3d(X, Y, Z, col = "red", size = 5, add = TRUE))
#'
#' # raster-based method
#'
#' chm = grid_canopy(las, 1, subcircle = 0.15)
#' chm = as.raster(chm)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = median, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, 5)
#'
#' raster::plot(chm, col = height.colors(30))
#' raster::plot(ttops, add = TRUE, col = "black", legend = FALSE)
tree_detection = function(x, ws, hmin = 2)
{
  UseMethod("tree_detection", x)
}

#'@export
tree_detection.LAS = function(x, ws, hmin = 2)
{
  if (ws <= 0) stop("ws should be stricly positive", call. = FALSE)
  if (hmin < 0) stop("hmin should be positive", call. = FALSE)

  . <- X <- Y <- Z <- NULL
  maxima = C_LocalMaximaPoints(x, ws, hmin)
  return(x@data[maxima, .(X,Y,Z)])
}

#'@export
tree_detection.lasmetrics = function(x, ws, hmin = 2)
{
  x = as.raster(x)
  return(tree_detection(x, ws, hmin))
}

#'@export
tree_detection.RasterLayer = function(x, ws, hmin = 2)
{
  xx <- raster::as.matrix(x)
  xx <- t(apply(xx, 2, rev))
  LM = tree_detection(xx, ws, hmin)
  LM = raster::raster(apply(LM,1,rev))
  raster::extent(LM) = raster::extent(x)
  return(LM)
}

#'@export
tree_detection.matrix = function(x, ws, hmin = 2)
{
  if (ws < 3)
    stop("ws should be equal or greater than 3", call. = FALSE)

  if (ws %% 2 == 0)
    stop("ws should be an odd number", call. = FALSE)

  x[is.na(x)] <- -Inf
  LM = C_LocalMaximaMatrix(x, ws, hmin)
  LM[LM == 0] <- NA
  return(LM)
}