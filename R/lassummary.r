# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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



#' Summary of LAS data
#'
#' This functions implements a \link[base:summary]{summary} method for LAS objects
#'
#' @aliases summary
#' @param object An object of the class \code{LAS}
#' @param \dots Unused (inherited from R base)
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' summary(lidar)
#'
#' @export
#' @seealso
#' \link[lidR:LAS]{Class LAS}
summary.LAS =	function(object, ...)
{
  size <- format(utils::object.size(object), units = "auto")

  if("pulseID" %in% names(object@data))
    npu = data.table::uniqueN(object@data$pulseID)
  else
    npu = NA

  s   = lasarea(object)
  npt = nrow(object@data)
  dpt = npt/s
  dpu = npu/s
  fie = names(object@data)
  ext = extent(object)

  cat("class        : LAS\n")
  cat("memory       :", size, "\n")
  cat("extent       :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("area         :", s, "m^2\n")
  cat("points       :", npt, "points\n")
  cat("pulses       :", npu , "pulses\n")
  cat("point density:", round(dpt, 2), "points/m^2\n")
  cat("pulse density:", round(dpu, 2), "pulses/m^2\n")
  cat("fields       :", length(fie), "\n")
  cat("field names  :", fie, "\n\n")
}

#' @rdname summary.LAS
#' @export
lassummary <- summary.LAS
