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


#' Plot a wireframe of a \code{RasterLayer} or a \code{lasmetrics} object
#'
#' @param x An object of the class \code{RasterLayer} or \code{lasmetrics}
#' @param y Unused (inherited from R base)
#' @param add logical. if TRUE, add to current 3D plot.
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl:surface3d]{surface3d}
#' @export
plot3d = function(x, y, add = FALSE, bg = "black", ...)
{
  inargs <- list(...)

  if(is(x, "lasmetrics"))
    x = as.raster(x)

  if(!is(x, "RasterLayer"))
    stop("Object not supported")

  mx =  t(apply(raster::as.matrix(x), 2, rev))
  x_  = sort(raster::xFromCol(x))
  y_  = sort(raster::yFromRow(x))

  if(!add)
    rgl::open3d()

  rgl::rgl.bg(color = bg)
  rgl::surface3d(x_, y_, mx, front="lines", col="white", ...)
}