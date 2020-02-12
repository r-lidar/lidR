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
#' This function is superseded by the algorithm \link{highest} usable in \link{decimate_points}
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
#' subset = filter_surfacepoints(las, 2)
#' plot(subset)
#'
#' @family filters
filter_surfacepoints = function(las, res)
{
  UseMethod("filter_surfacepoints", las)
}

#' @export
filter_surfacepoints.LAS = function(las, res)
{
  return(decimate_points(las, highest(res)))
}

#' @export
filter_surfacepoints.LAScluster = function(las, res)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- filter_surfacepoints(x, res)
  x <- filter_poi(x, buffer == 0)
  return(x)
}

#' @export
filter_surfacepoints.LAScatalog = function(las, res)
{
  opt_select(las)       <- "*"
  opt_chunk_buffer(las) <- res

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, filter_surfacepoints, res = res, .options = options)
  return(output)
}
