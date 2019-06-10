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

#' Filter the surface points
#'
#' This function is superseded by the algorithm \link{highest} usable in \code{lasfilterdecimate}
#'
#' @template param-las
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @template section-supported-option-lasfilter
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = lasfiltersurfacepoints(las, 2)
#' plot(subset)
#'
#' @family lasfilters
lasfiltersurfacepoints = function(las, res)
{
  UseMethod("lasfiltersurfacepoints", las)
}

#' @export
lasfiltersurfacepoints.LAS = function(las, res)
{
  return(lasfilterdecimate(las, highest(res)))
}

#' @export
lasfiltersurfacepoints.LAScluster = function(las, res)
{
  buffer <- NULL
  x <- suppressMessages(suppressWarnings(readLAS(las)))
  if (is.empty(x)) return(NULL)
  x <- lasfiltersurfacepoints(x, res)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasfiltersurfacepoints.LAScatalog = function(las, res)
{
  opt_select(las)       <- "*"
  opt_chunk_buffer(las) <- res

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_apply(las, lasfiltersurfacepoints, res = res, .options = options)
  output  <- unlist(output)
  ctg     <- suppressMessages(suppressWarnings(readLAScatalog(output)))

  opt_copy(ctg) <- las
  return(ctg)
}
