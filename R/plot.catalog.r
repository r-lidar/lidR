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
#' This functions implements a \link[graphics:plot]{plot} method for Catalog objects
#'
#' @param x A Catalog object
#' @param y Unused (inherited from base plot)
#' @param \dots inherited from base plot
#' @method plot Catalog
#' @export
#' @examples
#' \dontrun{
#'
#' catalog = catalog("<Path to a folder containing a set of .las files>")
#' plot(catalog)
#' }
plot.Catalog = function(x, y, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- filename <- NULL

  param = list(...)

  xmin = min(x$`Min X`)
  xmax = max(x$`Max X`)
  ymin = min(x$`Min Y`)
  ymax = max(x$`Max Y`)

  xcenter = (xmin + xmax)/2
  ycenter = (ymin + ymax)/2

  if (is.null(param$xlim))
    param$xlim = c(xmin, xmax)

  if (is.null(param$ylim))
    param$ylim = c(ymin, ymax)

  if (is.null(param$xlab))
    param$xlab = "X"

  if (is.null(param$ylab))
    param$ylab = "Y"

  if (is.null(param$asp))
    param$xlab = "X"

  if (is.null(param$asp))
    param$asp = 1

  if (is.null(param$col))
    param$col = "white"

  param$x = xcenter
  param$y = ycenter

  do.call(graphics::plot, param)
  graphics::rect(x$`Min X`, x$`Min Y`, x$`Max X`, x$`Max Y`)
}

