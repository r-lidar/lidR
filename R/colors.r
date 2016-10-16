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
#' @param ncolors numeric. The number of colors in the palette.
#' @param trim numeric.
#' @importFrom magrittr %>%
#' @importFrom stats quantile
set.colors = function(x, palette, ncolors = 50, trim = 1)
{

  if(trim < 1)
  {
    n = x %>% quantile(trim)
    x[x > n] = n
  }

  if(diff(range(x, na.rm = T)) == 0)
    colors = palette(ncolors)[1]
  else
    colors = palette(ncolors)[as.numeric(cut(x, breaks = ncolors))]

	return(colors)
}

#' height.colors
#'
#' Create a vector of n contiguous colors of elevations.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @importFrom grDevices colorRampPalette
#' @export height.colors
height.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("blue", "cyan2", "green3", "yellow", "red"))
  return(colfunc(n))
}

#' Color palette of green
#'
#' Create a vector of n contiguous colors of green from darkgreen to lightgreen.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @importFrom grDevices colorRampPalette
#' @export forest.colors
forest.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("darkgreen", "lightgreen"))
  return(colfunc(n))
}


#' Random palette
#'
#' Create a vector of n contiguous random colors.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @importFrom grDevices colorRampPalette
#' @export random.colors
random.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(sample(colors(distinct = T), n))
  return(colfunc(n))
}