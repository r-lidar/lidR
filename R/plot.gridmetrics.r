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



#' Plot an object of class gridmetrics in 2D
#'
#' This functions implements a \link[graphics:plot]{plot} method for a gridmetrics data.frame
#'
#' The \dots param provides additional arguments to \link[fields:image.plot]{image.plot}.
#'
#' @param x A data.frame or data.table of class gridmetrics.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param colorPalette function. A color palette function. Default is \code{height.colors} provided by the package lidR
#' @param \dots Supplementary parameters for \link[fields:image.plot]{image.plot}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' gridmetrics(lidar, 2, max(Z)) %>% plot
#'
#' # Mean height with 400 m^2 cells
#' gridmetrics(lidar, 20, mean(Z)) %>% plot
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
#' metrics = gridmetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID))
#'
#' plot(metrics, "hmean")
#' plot(metrics, "hmax")
#' plot(metrics, "imean")
#' @seealso
#' \link[lidR:gridmetrics]{gridmetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[fields:image.plot]{image.plot}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' \link[lidR:plot3d]{plot3d}
#' @importFrom fields image.plot
#' @export
#' @rdname plot.gridmetrics
#' @method plot gridmetrics
#' @importFrom magrittr %>%
plot.gridmetrics = function(x, z = NULL, colorPalette = height.colors, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM1")
    else
      z = names(x)[3]
  }

  mtx = as.matrix(x, z)
  X = rownames(mtx) %>% as.numeric
  Y = colnames(mtx) %>% as.numeric

  if(is.null(inargs$col))
    inargs$col = colorPalette(50)

  if(is.null(inargs$xlab))
    inargs$xlab = "X"

  if(is.null(inargs$ylab))
    inargs$ylab = "Y"

  if(is.null(inargs$asp))
    inargs$asp = 1

  do.call(fields::image.plot, c(list(x = X, y = Y, z = mtx), inargs))
}
