# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2018 Jean-Romain Roussel
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

#' Merge LAS* objects
#'
#' Merge LAS* objects
#'
#' @param ... LAS or LAScatalog objects
#' @name rbind
#' @export
rbind.LAS <- function(...)
{
  dots = list(...)
  names(dots) <- NULL
  assert_all_are_same_crs(dots)

  xscales <- sapply(dots, function(x) x@header@PHB[["X scale factor"]])
  yscales <- sapply(dots, function(x) x@header@PHB[["Y scale factor"]])
  zscales <- sapply(dots, function(x) x@header@PHB[["Z scale factor"]])
  xoffsets <- sapply(dots, function(x) x@header@PHB[["X offset"]])
  yoffsets <- sapply(dots, function(x) x@header@PHB[["Y offset"]])
  zoffsets <- sapply(dots, function(x) x@header@PHB[["Z offset"]])

  need_quantization <- FALSE
  if (length(unique(xscales)) != 1L || length(unique(yscales)) != 1L || length(unique(zscales)) != 1L ||
      length(unique(xoffsets)) != 1L || length(unique(yoffsets)) != 1L || length(unique(zoffsets)) != 1L)
  {
    warning("Different LAS objects have different scales and/or offsets. The first object was used as reference to quantize the others.", call. = FALSE)
    need_quantization <- TRUE
  }

  data <- data.table::rbindlist(lapply(dots, function(x) { x@data } ))

  if (need_quantization)
  {
    quantize(data[["X"]], xscales[1], xoffsets[1])
    quantize(data[["Y"]], yscales[1], yoffsets[1])
    quantize(data[["Z"]], zscales[1], zoffsets[1])
  }

  return(LAS(data, dots[[1]]@header, dots[[1]]@proj4string, index = dots[[1]]@index))
}

#' @name rbind
#' @export
rbind.LAScatalog <- function(...)
{
  dots <- list(...)
  names(dots) <- NULL
  assert_all_are_same_crs(dots)

  data <- data.table::rbindlist(lapply(dots, function(x) { x@data } ))

  ctg <- dots[[1]]
  ctg@data <- data

  minx <- min(data[["Min.X"]])
  miny <- min(data[["Min.Y"]])
  maxx <- min(data[["Max.X"]])
  maxy <- min(data[["Max.Y"]])
  ctg@bbox <- matrix(c(minx, maxx, miny, maxy), ncol = 2, byrow = T)

  chk = las_check(ctg, print = FALSE)
  for (msg in chk$warnings) warning(msg, call. = FALSE)
  for (msg in chk$errors) warning(msg, call. = FALSE)

  return(ctg)
}

