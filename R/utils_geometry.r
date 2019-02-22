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

#' Surface covered by a LAS* object.
#'
#' Surface covered by a \code{LAS*} object. For \code{LAS} point clouds it is computed based on the
#' convex hull of the points. For a \code{LAScatalog} it is computed as the sum of the bounding boxes
#' of the files. For overlapping tiles the value may be larger than the total covered area because
#' some regions are sampled twice.
#'
#' @param x An object of the class \code{LAS*}
#' @param ... unused
#'
#' @return numeric. The area of the object computed in the same units as the coordinate reference system
#' @export
#' @importMethodsFrom raster area
#' @rdname area
setGeneric("area", function(x, ...)
  standardGeneric("area"))

#' @export
#' @rdname area
setMethod("area", "LAS", function(x, ...)
{
  if (nrow(x@data) == 0)
    return(0)

  return(area_convex_hull(x@data$X, x@data$Y))
})

#' @rdname area
#' @export
setMethod("area", "LAScatalog",  function(x, ...)
{
  x <- x@data
  area <- sum((x$Max.X - x$Min.X) * (x$Max.Y - x$Min.Y))
  return(area)
})

area_convex_hull = function(x, y)
{
  stopifnot(length(x) == length(y))
  hull <- convex_hull(x, y)
  area <- polygon_area(hull$x, hull$y)
  return(area)
}

convex_hull = function(x, y)
{
  i <- grDevices::chull(x,y)
  i <- c(i, i[1])
  coords <- list(x = x[i], y = y[i])
  data.table::setDF(coords)
  return(coords)
}

polygon_area = function(x, y)
{
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!is.numeric(x) || !is.numeric(y) ) stop("Arguments 'x' and 'y' must be real")
  if (length(x) != length(y)) stop("Argument 'x' and 'y' must be of same size")

	area <- 0
	j <- length(x)

	for (i in 1:j)
	{
		area <- area + (x[j] + x[i])*(y[j] - y[i]);
		j <- i;
	}

	area <- abs(area*0.5)
	return(area)
}
