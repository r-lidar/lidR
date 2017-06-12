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



#' Compute the area covered by of a set a points.
#'
#' The area is computed by on the convex hull. If the data are not convex, the resulting area is only an approximation.
#'
#' @param .las An object of the class \code{LAS} or a numeric array of x coordinates
#' @return numeric. The area of the object computed in the same units as the coordinate reference system
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' lasarea(lidar)
#' @export
lasarea = function(.las)
{
  stopifnotlas(.las)

  hull = convex_hull(.las@data$X, .las@data$Y)
  area = polygon_area(hull$x, hull$y)
  area = round(area,1)
  return(area)
}
