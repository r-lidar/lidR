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



#' Clip LiDAR points
#'
#' Clip LiDAR points within a given geometry
#'
#' @param obj An object of class \code{LAS}
#' @param xleft	scalar. of left x position.
#' @param ybottom	scalar. of bottom y position.
#' @param xright scalar. of right x position.
#' @param ytop scalar. of top y position.
#' @param x	numerical array. x-coordinates of polygon
#' @param y	numerical array. y-coordinates of polygon
#' @param xcenter	scalar. x disc center
#' @param ycenter	calar. y disc center
#' @param radius a scalar. Disc radius
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                  xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @rdname clip
#' @export clipRectangle
#' @importFrom data.table between
setGeneric("clipRectangle", function(obj, xleft, ybottom, xright, ytop, inside = TRUE){standardGeneric("clipRectangle")})

#' @rdname clip
setMethod("clipRectangle", "LAS",
	function(obj, xleft, ybottom, xright, ytop, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(lasfilter(obj, between(X, xleft, xright), between(Y, ybottom, ytop)))
	  else
	    return(lasfilter(obj, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

	}
)



#' @export clipPolygon
#' @rdname clip
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})

#' @rdname clip
setMethod("clipPolygon", "LAS",
	function(obj, x, y, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(lasfilter(obj, points_in_polygon(x,y,X,Y)))
	  else
	    return(lasfilter(obj, !points_in_polygon(x,y,X,Y)))
	}
)


#' @export clipCircle
#' @rdname clip
setGeneric("clipCircle", function(obj, xcenter, ycenter, radius, inside = TRUE){standardGeneric("clipCircle")})

#' @rdname clip
setMethod("clipCircle", "LAS",
	function(obj, xcenter, ycenter, radius, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(lasfilter(obj, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
	  else
	    return(lasfilter(obj, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
	}
)
