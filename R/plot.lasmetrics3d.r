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



#' Plot voxelized LiDAR data
#'
#' This function implements a 3D plot method for 'lasmetrics3d' objects
#'
#' @param x An object of the class \code{'lasmetrics3d'}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to color the points. Default is Z coordinates. Or a vector of colors.
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background. Default is black.
#' @param trim numeric. Enables trimming of values when outliers break the color palette range.
#' Default is 1 meaning that the whole range of the values is used for the color palette.
#' 0.9 means that 10% of the highest values are not used to define the color palette.
#' In this case the values higher than the 90th percentile are set to the highest color. They are not removed
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d} if display method is "points"
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' voxels = grid_metrics3d(lidar, list(Imean = mean(Intensity)))
#' plot(voxels, color = "Imean", colorPalette = heat.colors(50), trim=0.99)
#' @seealso
#' \link[lidR:grid_metrics3d]{grid_metrics3d}
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @export
#' @method plot lasmetrics3d
plot.lasmetrics3d = function(x, y, color = "Z", colorPalette = height.colors(50), bg = "black", trim = 1, ...)
{
  inargs <- list(...)

  inargs$col = color

  if(length(color) == 1)
  {
    if(color %in% names(x))
    {
      data = unlist(x[,color, with = FALSE])

      if(is.numeric(data))
      {
        inargs$col = set.colors(data, colorPalette, trim)
        inargs$col[is.na(inargs$col)] = "lightgray"
      }
      else if(is.character(data))
        inargs$col = data
    }
  }

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, c(list(x=x$X, y=x$Y, z=x$Z), inargs))
}
