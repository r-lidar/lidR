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



#' Canopy surface model
#'
#' Creates a canopy surface model using a LiDAR point cloud. For each pixel the function
#' returns the highest point found (point-to-raster). This basic method could be improved
#' by replacing each LiDAR return with a small disc. An interpolation for empty pixels is
#' also available.
#'
#' The algorithm relies on a point-to-raster approach. For each pixel the elevation of the
#' highest point is found and attributed to this pixel. This method implies that the resulting
#' surface model can contain empty pixels. Those 'holes' can be filled by interpolation.
#' Internally, the interpolation is based on the same method used in the function
#' \link[lidR:grid_terrain]{grid_terrain}. Therefore the documentation for
#' \link[lidR:grid_terrain]{grid_terrain} is also applicable to this function (see also
#' examples).\cr\cr
#' The 'subcircle' tweak replaces each point with 8 points around the original one. This allows
#' for virtual 'emulation' of the fact that a lidar point is not a point as such, but more
#' realistically a disc. This tweak densifies the point cloud and the resulting canopy model is
#' smoother and contains fewer 'pits' and empty pixels.
#'
#' @template LAScatalog
#'
#' @template param-las
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is
#' 2 meters i.e. 4 square meters.
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#' @template param-ellipsis-select-filter

#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Local maximum algorithm with a resolution of 2 meters
#' chm = grid_canopy(lidar, 2)
#' plot(chm)
#'
#' # Local maximum algorithm with a resolution of 1 meter replacing each
#' # point by a 20 cm radius circle of 8 points
#' chm = grid_canopy(lidar, 1, 0.2)
#' plot(chm)
#'
#' # Local maximum algorithm with a resolution of 1 meter replacing each
#' # point by a 10 cm radius circle of 8 points and interpolating the empty
#' # pixels using the 3-nearest neighbours and an inverse-distance weighting.
#' chm = grid_canopy (lidar, 1, subcircle = 0.1, na.fill = "knnidw", k = 3, p = 2)
#' plot(chm)
#'
#' \dontrun{
#' chm = grid_canopy(lidar, 1, na.fill = "knnidw", k = 3)
#' plot(chm)
#' chm = grid_canopy(lidar, 1, subcircle = 0.1, na.fill = "delaunay")
#' plot(chm)
#' }
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' @export
grid_canopy = function(las, res = 2, subcircle = 0, ...)
{
  UseMethod("grid_canopy", las)
}

#' @export
grid_canopy.LAS = function(las, res = 2, subcircle = 0, ...)
{
  . <- X <- Y <- Z <- NULL

  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)
  assertive::assert_is_a_number(subcircle)
  assertive::assert_all_are_non_negative(subcircle)

  verbose("Gridding highest points in each cell...")

  layout <- make_overlay_raster(las, res, subcircle = subcircle)
  names(layout) <- "Z"
  bbox <- raster::extent(layout)

  dsm <- C_grid_canopy(las, raster::as.matrix(bbox), res, subcircle)
  dsm <- t(dsm)

  if (!all(dim(layout)[1:2] == dim(dsm)))
    stop("Internal error: matrix returned at the C++ level don't match with the layout. Please report this bug.", call. = FALSE)

  suppressWarnings(layout[] <- dsm)
  return(layout)

  # if (na.fill != "none")
  # {
  #   verbose("Interpolating empty cells...")
  #
  #   hull = convex_hull(las@data$X, las@data$Y)
  #
  #   # buffer around convex hull
  #   sphull = sp::Polygon(hull)
  #   sphull = sp::SpatialPolygons(list(sp::Polygons(list(sphull), "null")))
  #   bhull  = rgeos::gBuffer(sphull, width = res)
  #
  #   # mask
  #   mask = raster::mask(layout, bhull)
  #
  #
  #   z = interpolate(dsm[!is.na(Z)], dsm[is.na(Z)], method = na.fill, ...)
  #
  #   dsm[is.na(Z), Z := z]
  #
  #   as.lasmetrics(dsm, res)
  # }
}

#' @export
grid_canopy.LAScluster = function(las, res = 2, subcircle = 0, ...)
{
  x = readLAS(las, ...)
  if (is.null(x)) return(NULL)
  bbox = raster::extent(as.numeric(las@bbox))
  metrics = grid_canopy(x, res, subcircle)
  metrics = raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
grid_canopy.LAScatalog = function(las, res = 2, subcircle = 0, ...)
{
  buffer(las)   <- 0.1*res
  output        <- catalog_apply2(las, grid_canopy, res = res, subcircle = subcircle, ..., need_buffer = FALSE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (output_files(las) != "")  return(unlist(output))

  # Outputs have been return in R objects. Merge the outptus in a single object
  names         <- names(output[[1]])
  factor        <- output[[1]]@data@isfactor
  output        <- do.call(raster::merge, output)
  output@crs    <- las@proj4string
  names(output) <- names
  if (is(output, "RasterBrick")) colnames(output@data@values) <- names
  return(output)
}
