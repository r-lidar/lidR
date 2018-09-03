# ===============================================================================
#
# PROGRAMMERS:
#
# andrew.sanchezmeador@nau.edu - https://github.com/bi0m3trics
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel.
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

#' Snag classification
#'
#' Snag classification/segmentation using several possible algorithms (see details).
#' The function attributes to each point of the point cloud a number identifying a
#' snag class (\code{snagCls} column). The classification/segmentation is done at the point
#' cloud level and there is currently only one algorithm implemented (which uses LiDAR intensity
#' thresholds and specified neighborhoods to differentiate bole and branch from foliage points
#' (see details).
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param algorithm function.  An algorithm for snag segmentation. At present, lidR have only
#' \link{wing2015}.
#' @param attribute character. The original LAS object is automatically updated by the function. A new
#' column is added. This parameter is the name of this new column.
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @export
#' @family Snags Segmentation
lassnags = function (las, algorithm, attribute = "snagCls")
{
  UseMethod("lassnags", las)
}

#' @export
#' @export
lassnags.LAS = function (las, algorithm, attribute = "snagCls")
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "SnagsSegmentation"))
    stop("The algorithm is not an algorithm for snags segmentation.", call. = FALSE)

  stopif_forbidden_name(attribute)

  lidR.context <- "lassnags"
  snags <- algorithm(las)

  lasaddextrabytes(las, snags, attribute, "Number identifying a snag class")
  return(invisible(las))
}

#' @export
lassnags.LAScluster = function (las, algorithm, attribute = "snagCls")
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  lassnags(x, algorithm)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lassnags.LAScatalog = function (las, algorithm, attribute = "snagCls")
{
  set_select(las) <- "*"

  output      <- catalog_apply2(las, lassnags,  algorithm = algorithm, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}
