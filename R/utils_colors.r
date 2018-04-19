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

#' Automatic colorization
#'
#' Attribute a color to each element of a vector
#'
#' @param x A vector
#' @param palette function. A color palette function. Default is \code{height.colors} provided by the package lidR
#' @param trim numeric.
#' @keywords internal
set.colors = function(x, palette, trim = 1)
{
  ncolors = length(palette)

  if (trim < 1)
  {
    n = x %>% stats::quantile(trim)
    x[x > n] = n
  }

  minx = min(x, na.rm = T)
  maxx = max(x, na.rm = T)

  if (maxx-minx == 0)
    colors = palette[1]
  else
  {
    idx = findInterval(x, seq(minx, maxx, length.out = ncolors))
    colors = palette[idx]
  }

  return(colors)
}

#' Palettes
#'
#' Create a vector of n contiguous (or not) colors
#'
#' @param n The number of colors (> 1) to be in the palette
#' @family lidrpalettes
#' @name lidrpalettes
#' @seealso \link[grDevices:heat.colors]{grDevices::Palettes}
NULL

#' @export
#' @rdname lidrpalettes
#' @family lidrpalettes
height.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))
  return(colfunc(n))
}

#' @export
#' @rdname lidrpalettes
#' @family lidrpalettes
forest.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("darkgreen", "lightgreen"))
  return(colfunc(n))
}


#' @export
#' @rdname lidrpalettes
#' @family lidrpalettes
random.colors = function(n)
{
  h = stats::runif(n, 0, 1);
  s = stats::runif(n, 0.2, 1);
  l = stats::runif(n, 0.8, 1);

  return(grDevices::hsv(h,s,l))
}

#' @export
#' @rdname lidrpalettes
#' @family lidrpalettes
pastel.colors = function(n)
{
  h = stats::runif(n, 0, 360);
  c = stats::runif(n, 42, 98);
  l = stats::runif(n, 40, 90);

  return(grDevices::hcl(h,c,l))
}