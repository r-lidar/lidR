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



#' Clip LiDAR points within a rectangle
#'
#' Clip LiDAR points within a rectangle
#'
#' @param obj An object of class \code{LAS}
#' @param xleft	a scalar of left x position.
#' @param ybottom	a scalar of bottom y position.
#' @param xright a scalar of right x position.
#' @param ytop a scalar of top y position.
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{LAS}
#' @family clip
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                  xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @export clipRectangle
#' @importFrom data.table between
setGeneric("clipRectangle", function(obj, xleft, ybottom, xright, ytop, inside = TRUE){standardGeneric("clipRectangle")})

#' @rdname clipRectangle
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

#' Clip LiDAR points within a polygon
#'
#' Clip LiDAR points within a polygon
#'
#' @aliases clipPolygon
#' @param obj An object of class \code{LAS}
#' @param x	numerical array of x-coordinates of polygon
#' @param y	numerical array of y-coordinates of polygon
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{LAS}
#' @family clip
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' subset = lidar %>% clipPolygon(x=c(685000, 685200, 685050),
#'                                y=c(5018000, 5018100, 5018200))
#'
#'
#' plot(subset)
#' @export clipPolygon
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})

#' @rdname clipPolygon
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

#' Clip LiDAR points within a disc
#'
#' Clip LiDAR points within a disc
#'
#' @aliases clipCircle
#' @param obj An object of class \code{LAS}
#' @param xcenter	a scalar. x disc center
#' @param ycenter	a scalar. y disc center
#' @param radius a scalar. Disc radius
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{LAS}
#' @family clip
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' subset = lidar %>% clipCircle(685000, 5018000, 25)
#'
#' plot(subset)
#' @export clipCircle
setGeneric("clipCircle", function(obj, xcenter, ycenter, radius, inside = TRUE){standardGeneric("clipCircle")})

#' @rdname clipCircle
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
