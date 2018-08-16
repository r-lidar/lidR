# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https:#github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
#
# This file is part of lidRExtra R package.
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
# along with this program.  If not, see <http:#www.gnu.org/licenses/>
#
# ===============================================================================


#' Classify points as ground or not ground
#'
#' Classify points as ground or not ground with several possible algorithms. The function is a wrapper
#' around all the existing methods. The functions update the field \code{Classification} of the LAS
#' input object. The points classified as 'ground' are assigned a value of 2 according to las
#' specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#'
#' @param las a LAS object.
#' @param algorithm character. The name of an algorithm. Currently \code{"pmf"} and \code{csf} are
#' supported (see related sections).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' of the algorithms).
#'
#' @template  return-lasground
#'
#' @export
#' @family lasground
#' @importFrom data.table :=
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyzrn")
#'
#' ws = seq(3,12, 3)
#' th = seq(0.1, 1.5, length.out = length(ws))
#'
#' lasground(las, "pmf", ws, th)
#'
#' plot(las, color = "Classification")
lasground = function(las, algorithm,  ...)
{
  if (algorithm == "pmf")
    lasground_pmf(las, ...)
  else if (algorithm == "csf")
    lasground_csf(las, ...)
  else
    stop("This algorithm does not exist.", call. = F)
}
