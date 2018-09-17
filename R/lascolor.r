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


#' Compute the color from RGB attributes
#'
#' Compute the hexadecimal name of a color from RGB attributes. A column 'color' is added in \code{LAS}
#' object.
#'
#' @param las A LAS object
#' @param nbits the number of bits used to store the R G and B attributes. Default is 16 bits i.e.
#' each channel ranged between 0 and 65535 as defined in LAS specifications. But sometime this rule
#' is not respected. You can force the default behavior. A 8 bit color ranged between 0 and 255.
#'
#' @return Nothing. The original LAS object is updated by reference (using side effect) to avoid any
#' copy in memory of the point cloud.
#'
#' @export
lascolor = function(las, nbits = 16)
{
  stopifnotlas(las)
  assertive::assert_is_a_number(nbits)
  assertive::assert_all_are_in_closed_range(nbits, 0, 16)

  color <-R <- G <- B <- NULL
  maxcol = 2^nbits-1

  if(!all(c("R", "G", "B") %in% names(las@data)))
    stop("No 'RGB' attributes found. 'color' cannot be computed from this file.", call. = FALSE)

  las@data[, color := grDevices::rgb(R/maxcol, G/maxcol, B/maxcol)]

  return(invisible(las))
}