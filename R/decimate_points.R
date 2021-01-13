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


#' Decimate a LAS object
#'
#' Reduce the number of points using several possible algorithms.
#'
#' @template param-las
#' @param algorithm function. An algorithm of point decimation. \code{lidR} have: \link{random},
#' \link{homogenize} and \link{highest}.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasfilter
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Select points randomly to reach an overall density of 1
#' thinned1 = decimate_points(las, random(1))
#' #plot(grid_density(las))
#' #plot(grid_density(thinned1))
#'
#' # Select points randomly to reach an homogeneous density of 1
#' thinned2 = decimate_points(las, homogenize(1,5))
#' #plot(grid_density(thinned2))
#'
#' # Select the highest point within each pixel of an overlayed grid
#' thinned3 = decimate_points(las, highest(5))
#' #plot(thinned3)
decimate_points = function(las, algorithm)
{
  UseMethod("decimate_points", las)
}

#' @export
decimate_points.LAS = function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dec(algorithm)
  lidR.context <- "decimate_points"
  selected <- algorithm(las)
  return(LAS(las@data[selected], las@header, las@proj4string, check = FALSE, index = las@index))
}

#' @export
decimate_points.LAScluster = function(las, algorithm)
{
  buffer <- NULL
  x <- suppressMessages(suppressWarnings(readLAS(las)))
  if (is.empty(x)) return(NULL)
  x <- decimate_points(x, algorithm)
  x <- filter_poi(x, buffer == 0)
  return(x)
}

#' @export
decimate_points.LAScatalog = function(las, algorithm)
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dec(algorithm)

  # Enforce some options
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  e <- environment(algorithm)
  if (!is.null(e[["res"]])) opt_chunk_buffer(las) <- e[["res"]]

  # Processing
  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, decimate_points, algorithm = algorithm, .options = options)
  return(output)
}
