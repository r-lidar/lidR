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



#' Plot an object of class lasmetrics in 2D
#'
#' This functions implements a plot method for a \code{lasmetrics} data.frame
#'
#' The \dots param provides additional arguments to \link[raster:plot]{plot}.
#'
#' @param x A data.frame or data.table of class \code{lasmetrics}.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param colorPalette function. A color palette function. Default is \code{height.colors} provided by the package lidR
#' @param \dots Supplementary parameters for \link[raster:plot]{plot}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' grid_canopy(lidar) %>% plot
#'
#' # Mean height with 400 m^2 cells
#' grid_metrics(lidar, mean(Z)) %>% plot
#'
#' # Define your own metric function
#' myMetrics = function(z, i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         hmean   = mean(z),
#'         hmax    = max(z),
#'         imean   = mean(i),
#'         angle   = mean(abs(angle))
#'         )
#'
#'    return(ret)
#'  }
#'
#' metrics = grid_metrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID))
#'
#' plot(metrics, "hmean")
#' plot(metrics, "hmax")
#' plot(metrics, "imean")
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:grid_canopy]{grid_canopy}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @export
#' @method plot lasmetrics
plot.lasmetrics = function(x, z = NULL, colorPalette = height.colors, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM1")
    else
      z = names(x)[3]
  }

  mtx = as.raster(x, z)

  if(is.null(inargs$col))
    inargs$col = colorPalette(50)

  if(is.null(inargs$xlab))
    inargs$xlab = "X"

  if(is.null(inargs$ylab))
    inargs$ylab = "Y"

  do.call(raster::plot, c(list(x = mtx), inargs))
}
