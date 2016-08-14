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
#' This functions implements a 3D plot method for voxels objects
#'
#' By default the function plots points for fast display purposes. It can also plot real voxels.
#' @aliases plot.voxels
#' @param x An object of the class \code{voxels}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to color the points. Default is Z coordinates. Or a vector of colors.
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background. Default is black.
#' @param display character. By default is "points" to plot voxels as points for fast display.
#' Could be set to "cubes" to display nice shaded cube. But "cubes" method is very very slow.
#' It cannot be use for a large number of voxels.
#' @param trim numeric. Enables trimming of values when outliers break the color palette range.
#' Default is 1 meaning that the whole range of the values is used for the color palette.
#' 0.9 means thant 10% of the hightest values are not used to defined the colors palette.
#' In this case the values higher that the 90th percentile are set to the highest color. They are not removed
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d} if display method is "points"
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' voxels = voxelize(lidar, 1, list(Imean = mean(Intensity)))
#' plot(voxels, color = "Imean", colorPalette = heat.colors, trim=0.99)
#' @seealso
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' \link[lidR:voxelize]{voxelize}
#' @export
#' @importFrom rgl points3d open3d rgl.bg
#' @importFrom grDevices heat.colors terrain.colors topo.colors
#' @importFrom magrittr %$%
plot.voxels = function(x, y, color = "Z", colorPalette = height.colors, bg = "black", display = "points", trim = 1, ...)
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
        inargs$col = set.colors(data, colorPalette, 50, trim)
        inargs$col[is.na(inargs$col)] = "lightgray"
      }
      else if(is.character(data))
        inargs$col = data
    }
  }

  if(display == "points")
  {
    rgl::open3d()
    rgl::rgl.bg(color = bg)
    do.call(rgl::points3d, c(list(x=x$X, y=x$Y, z=x$Z), inargs))
  }
  else if(display == "cubes")
  {
    rgl::open3d()
    rgl::rgl.bg(color = bg)
    x %$% cube(x$X, x$Y, x$Z, inargs$col, scale = attr(x, "res"))
  }
  else
    lidRError("VOX1")
}
