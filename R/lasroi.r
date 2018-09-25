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



# Select a region of interest interactively
#
# Select a rectangular region of interest interactively using the mouse
#
# \code{lasroi} enables the user to select a region of interest (ROI) by drawing
# a rectangle with the mouse
# @aliases lasroi
# @param .las An object of class \code{LAS}
# @param \dots Optional parameters for the plot function
# @return An object of class \code{LAS}
# @examples
#\dontrun{
# LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
# lidar = readLAS(LASfile)
# subset = lasroi(lidar)
# }
# @export
# lasroi = function(.las, ...)
# {
#   X <- Y <- NULL
#
#   stopifnotlas(.las)
#
#   plot(.las, ...)
#   rgl::view3d(0,0)
#
#   f = rgl::select3d()
#   keep = f(.las@data$X, .las@data$Y,0)
#   rgl::rgl.close()
#
#   out = lasfilter(.las, keep)
#   plot.LAS(out, ...)
#
#   return(out)
# }
