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

#' @export
#' @rdname grid_canopy
grid_canopy_p2r = function(las, res = 2, subcircle = 0)
{
  UseMethod("grid_canopy_p2r", las)
}

#' @export
grid_canopy_p2r.LAS = function(las, res = 2, subcircle = 0)
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
grid_canopy_p2r.LAScluster = function(las, res = 2, subcircle = 0)
{
  x = readLAS(las)
  if (is.null(x)) return(NULL)
  bbox = raster::extent(as.numeric(las@bbox))
  metrics = grid_canopy(x, res, subcircle)
  metrics = raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
grid_canopy_p2r.LAScatalog = function(las, res = 2, subcircle = 0)
{
  buffer(las)   <- 0.1*res
  las@input_options$select <- "xyz"

  output        <- catalog_apply2(las, grid_canopy_p2r, res = res, subcircle = subcircle, need_buffer = FALSE, check_alignement = TRUE, drop_null = TRUE)

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