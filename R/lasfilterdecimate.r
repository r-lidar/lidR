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
#' # Select point randomly to reach a density of 1 on the overall.
#' thinned1 = lasfilterdecimate(las, random(1))
#' plot(grid_density(las))
#' plot(grid_density(thinned1))
#'
#' # Select point randomly to reach an homogeneous density of 1
#' thinned2 = lasfilterdecimate(las, homogenize(1,5))
#' plot(grid_density(thinned2))
#'
#' # Select the highest point within each pixel of an overlayed grid
#' thinned3 = lasfilterdecimate(las, highest(5))
#' plot(thinned3)
lasfilterdecimate = function(las, algorithm)
{
  UseMethod("lasfilterdecimate", las)
}

#' @export
lasfilterdecimate.LAS = function(las, algorithm)
{
  pulseID <- gpstime <- NULL

  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "PointCloudDecimation"))
    stop("The algorithm is not an algorithm for point cloud decimation.", call. = FALSE)

  lidR.context <- "lasfilterdecimate"
  selected <- algorithm(las)

  return(LAS(las@data[selected], las@header, las@proj4string))
}

#' @export
lasfilterdecimate.LAScluster = function(las, algorithm)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- lasfilterdecimate(x, algorithm)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasfilterdecimate.LAScatalog = function(las, algorithm)
{
  set_select(las) <- "*"

  output      <- catalog_apply2(las, lasfilterdecimate, algorithm = algorithm, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}
