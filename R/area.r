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
#' The area is computed with a convex hull. It is only an approximation if the
#' shape of the data is not convex.
#'
#' @aliases area
#' @param x An object of the class \code{LAS} or a numeric array of x coordinates
#' @param y numeric array of y coordinates
#' @return numeric. The area of the object computed with a convex hull in coordinates units
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' area(lidar)
#' @export area
setGeneric("area", function(x, y){standardGeneric("area")})

#' @rdname area
setMethod("area", "LAS",
	function(x, y = NULL)
	{
		hull = convex_hull(x@data$X, x@data$Y)
		area = polygon_area(hull$x, hull$y)
		area = round(area,1)
		return(area)
	}
)

#' @rdname area
setMethod("area", c("numeric", "numeric"),
  function(x, y)
  {
    hull = convex_hull(x, y)
    area = polygon_area(hull$x, hull$y)
    area = round(area,1)
    return(area)
  }
)
