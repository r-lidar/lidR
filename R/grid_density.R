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


#' Map the pulse or point density
#'
#' Creates a map of the point density. If a "pulseID" attribute is found, also returns a map of the pulse
#' density.
#'
#' @template param-las
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 = 16
#' square meters.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-LayerBrick
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile,  filter = "-inside 684800 5017800 684900 5017900")
#'
#' d <- grid_density(las, 5)
#' plot(d)
#'
#' las <- retrieve_pulses(las)
#' d <- grid_density(las)
#' plot(d)
grid_density = function(las, res = 4)
{
  UseMethod("grid_density", las)
}

#' @export
grid_density.LAS = function(las, res = 4)
{
  if (is(res, "RasterLayer"))
    resolution = raster::res(res)[1]
  else
    resolution = res

  if (!"pulseID" %in% names(las@data))
  {
    layout <- rOverlay(las, res, buffer = 0)
    count <- C_rasterize(las, layout, FALSE, 3L)
    layout[] <- count
    X <- layout
  }
  else
    X <- grid_metrics(las, ~list(point_density = .N, pulse_density = length(unique(pulseID))), res)

  X[is.na(X)] <- 0
  return(X/resolution^2)
}

#' @export
grid_density.LAScluster = function(las, res = 4)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox <- raster::extent(las)
  dsm  <- grid_density(x, res)
  dsm  <- raster::crop(dsm, bbox)
  raster::crs(dsm) <- crs(x) # patch for raster not updated with rgal 1.5-8
  return(dsm)
}

#' @export
grid_density.LAScatalog = function(las, res = 4)
{
  # Defensive programming
  if (!is_a_number(res) && !is(res, "RasterLayer"))  stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)

  # Enforce some options
  opt_select(las) <- "xyz"
  if (opt_wall_to_wall(las))
    opt_chunk_buffer(las) <- 2

  # Compute the alignment option including the case when res is a RasterLayer
  alignment   <- list(res = res, start = c(0,0))
  if (is(res, "RasterLayer"))
  {
    ext       <- raster::extent(res)
    r         <- raster::res(res)[1]
    las       <- catalog_intersect(las, res)
    start     <- c(ext@xmin, ext@ymin)
    alignment <- list(res = r, start = start)
  }

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_apply(las, grid_density, res = res, .options = options)
  return(output)
}

