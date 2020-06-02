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
#' Creates a digital surface model (DSM) using several possible algorithms. If the user provides a
#' normalised point cloud, the output is indeed a canopy height model (CHM).
#'
#' @template param-las
#'
#' @param algorithm function. A function that implements an algorithm to compute a digital surface model.
#' \code{lidR} implements \link{p2r}, \link{dsmtin}, \link{pitfree} (see respective documentation and examples).
#'
#' @template param-res-grid
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-Layer
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Points-to-raster algorithm with a resolution of 1 meter
#' chm <- grid_canopy(las, res = 1, p2r())
#' plot(chm, col = col)
#'
#' # Points-to-raster algorithm with a resolution of 0.5 meters replacing each
#' # point by a 20-cm radius circle of 8 points
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
grid_canopy = function(las, res, algorithm)
{
  UseMethod("grid_canopy", las)
}

#' @export
grid_canopy.LAS = function(las, res, algorithm)
{
  # Defensive programming
  if (!is_a_number(res) && !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dsm(algorithm)
  assert_las_is_not_empty(las)

  # Some algorithm have an extra option 'subscircle' that need to buffer the layout
  # Must be rewritten because it is a hack !
  subcircle <- as.list(environment(algorithm))$subcircle
  subcircle <- if (is.null(subcircle)) 0 else subcircle

  # Compute the RasterLayer that encompass the point cloud
  layout <- rOverlay(las, res, buffer = subcircle)

  # Compute the elevation for each cells
  lidR.context <- "grid_canopy"
  z <- algorithm(las, layout)

  suppressWarnings(layout[] <- z)
  names(layout) <- "Z"
  return(layout)
}

#' @export
grid_canopy.LAScluster = function(las, res, algorithm)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox <- raster::extent(las)
  dsm  <- grid_canopy(x, res, algorithm)
  dsm  <- raster::crop(dsm, bbox)
  raster::crs(dsm) <- crs(x) # patch for raster not updated with rgal 1.5-8
  return(dsm)
}

#' @export
grid_canopy.LAScatalog = function(las, res, algorithm)
{
  # Defensive programming
  if (!is_a_number(res) && !is(res, "RasterLayer"))  stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dsm(algorithm)

  # Enforce some options
  opt_select(las)       <- "xyzr"
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
  output  <- catalog_apply(las, grid_canopy, res = res, algorithm = algorithm, .options = options)
  return(output)
}
