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



#' Vertical Complexity Index
#'
#' A fixed normalization of the entropy function (see references)
#' @param z vector of z coordinates
#' @param by numeric. The thickness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function vci.
#' @return A number between 0 and 1
#' @seealso
#' \link[lidR:entropy]{entropy}
#' @examples
#' z = runif(10000, 0, 10)
#'

#' VCI(z, by = 1, zmax = 20)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be closer to 0.
#' VCI(z, by = 1, zmax = 20)
#' @references van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using LAS-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269. Retrieved from <Go to ISI>://WOS:000288052100009
#' @export VCI
VCI = function(z, zmax, by = 1)
{
  z = z[z < zmax]

  return(entropy(z, by, zmax))
}
