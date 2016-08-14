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



#' Plot a Catalog object
#'
#' This functions implements a \link[graphics:plot]{plot} method for a Catalog objects
#'
#' @param x A Catalog object
#' @param y Unused (inherited from base plot)
#' @param \dots Unused (inherited from base plot)
#' @export
#' @examples
#' \dontrun{
#'
#' catalog = Catalog("<Path to a folder containing a set of .las files>")
#' plot(catalog)
#' }
#' @importFrom graphics plot rect text
#' @importFrom magrittr %$%
plot.Catalog = function(x, y, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- filename <- NULL

  headers = x@headers

  xmin = min(headers$Min.X)
  xmax = max(headers$Max.X)
  ymin = min(headers$Min.Y)
  ymax = max(headers$Max.Y)

  xcenter = (headers$Min.X+headers$Max.X)/2
  ycenter = (headers$Min.Y+headers$Max.Y)/2

  graphics::plot(xcenter, ycenter, xlim=c(xmin, xmax), col="white", ylim = c(ymin, ymax), asp=1, xlab="X", ylab="Y")
  headers %$% graphics::rect(Min.X, Min.Y, Max.X, Max.Y)
}

