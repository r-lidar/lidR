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



#' Get the evelation of the ground for given coordinates
#'
#' The algorithm use the k-nearest ground points to the input coordinates to
#' interpol the elevations at the points of interest. The interpolation is done by kriging
#' using a invert distance kernel.
#'
#' @param las a LAS objet
#' @param coord data.frame containing the at least the coordinates of the points of
#' interest in the columns named X and Y.
#' @param k numeric. The number of nearest neighbours
#' @return Numeric. The predicted elevations.
#' @export
#' @seealso
#' \link[kknn:kknn]{kknn}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @importFrom data.table copy :=
#' @importFrom plyr round_any
#' @importFrom kknn kknn
setGeneric("get_ground_elevation", function(las, coord, k = 3L){standardGeneric("get_ground_elevation")})

#' @rdname get_ground_elevation
setMethod("get_ground_elevation", "LAS",
  function(las, coord, k = 3L)
  {
    fields = names(coord)

    if( !"X" %in% fields | !"Y" %in% fields)
      stop("Parameter coord does not have a column named X or Y")

    ground = getGround(las)
    Zg = kknn::kknn(Z~X+Y, ground@data, coord, k = k, distance = 1, kernel = "inv")$fitted.values

    return(Zg)
  }
)
