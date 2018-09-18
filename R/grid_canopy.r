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
#'
#' @param algorithm function. A function that implements an algorithm to compute a digital surface model.
#' \code{lidR} have \link{p2r}, \link{dsmtin}, \link{pitfree} (see respective documentations and exemples).
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
grid_canopy = function(las, res, algorithm)
{
  if(!assertive::is_a_number(res) & !is(res, "RasterLayer"))
    stop("res is not a number or a RasterLayer", call. = FALSE)

  if(assertive::is_a_number(res))
    assertive::assert_all_are_non_negative(res)

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
  if (is(res, "RasterLayer"))
  {
    ext = raster::extent(res)
    keep = with(las@data, !(`Min X` >= ext@xmax | `Max X` <= ext@xmin | `Min Y` >= ext@ymax | `Max Y` <= ext@ymin))
    las = las[keep,]
  }

  set_select(las) <- "xyzr"
  output <- catalog_apply2(las, grid_canopy, res = res, algorithm = algorithm, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (get_output_files(las) != "")  return(unlist(output))

  if (get_output_files(las) != "")                  # Outputs have been written in files. Return a virtual raster mosaic
    return(build_vrt(output, "grid_canopy"))
  else                                              # Outputs have been returned in R objects. Merge the outputs in a single object
    return(merge_rasters(output))
}