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


#' Plot a wireframe of a RasterLayer object
#'
#' @param x An object of the class RasterLayer
#' @param y Unused (inherited from R base)
#' @param add logical. if TRUE, add to current 3D plot.
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl:surface3d]{surface3d}
#' @export
plot3d = function(x, y, add = FALSE, bg = "black", ...)
{
  inargs <- list(...)

  mx = raster::as.matrix(x) %>% apply(2, rev) %>% t
  coord = sp::coordinates(x)
  x = coord[,1] %>% unique %>% sort
  y = coord[,2] %>% unique %>% sort

  if(!add) rgl::open3d()

  rgl::rgl.bg(color = bg)

  rgl::surface3d(x, y, mx, front="lines", col="white", ...)
}