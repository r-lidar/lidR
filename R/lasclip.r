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
#' @param .las An object of class \code{LAS}
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
#' subset = lidar %>% lasclipRectangle(xleft=684850, ybottom=5017850,
#'                                  xright=684900, ytop =5017900)
#'
#' plot(subset)
#' @name lasclip
NULL

#' @export lasclipRectangle
#' @rdname lasclip
lasclipRectangle = function(.las, xleft, ybottom, xright, ytop, inside = TRUE)
{
  X <- Y <- NULL

  if(inside)
    return(lasfilter(.las, between(X, xleft, xright), between(Y, ybottom, ytop)))
  else
    return(lasfilter(.las, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

}

#' @export lasclipPolygon
#' @rdname lasclip
lasclipPolygon = function(.las, x, y, inside = TRUE)
{
  X <- Y <- NULL

  if(inside)
    return(lasfilter(.las, points_in_polygon(x,y,X,Y)))
  else
    return(lasfilter(.las, !points_in_polygon(x,y,X,Y)))
}


#' @export lasclipCircle
#' @rdname lasclip
lasclipCircle = function(.las, xcenter, ycenter, radius, inside = TRUE)
{
  X <- Y <- NULL

  if(inside)
    return(lasfilter(.las, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
  else
    return(lasfilter(.las, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
}
