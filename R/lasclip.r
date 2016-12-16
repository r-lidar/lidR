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
#' Clip LiDAR points within a given geometry and convenient wrappers most common geometries
#'
#' @param .las An object of class \code{LAS}
#' @param geometry charaters. name of a geometry. Can be \code{"circle"}, \code{"rectangle"},
#' \code{"polygon"}, \code{"cuboid"} or \code{"sphere"}
#' @param coord matrix or data.frame. The coordinates of the minimum points requiered to fully
#' describe the geometry. For circle a 1-by-3 matrix, for rectangle a 2-by-2 matrix, for polygon n-by-2
#' matrix, for cuboid 2-by-3 matrix and for sphere a 1-by-4 matrix
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
#' lidar = readLAS(LASfile)
#'
#' subset = lidar %>% lasclipRectangle(xleft  = 684850, ybottom = 5017850,
#'                                     xright = 684900, ytop    = 5017900)
#' plot(subset)
#'
#' msphere = matrix(c(684850, 5017850, 10, 10), ncol = 4)
#' subset = lidar %>% lasclip("sphere", msphere)
#' plot(subset)
#'
#' mrect = matrix(c(684850, 684900, 5017850, 5017900), ncol = 2)
#' subset = lidar %>% lasclip("rectangle", mrect)
#' @name lasclip
#' @export
lasclip = function(.las, geometry, coord, inside = TRUE)
{
  if(is.matrix(coord))
    coord = data.table::as.data.table(coord)
  else if(is.data.frame(coord))
    data.table::setDT(coord)
  else if(!data.table::is.data.table(coord))
    stop("'coord' must be a matrix, a data.frame or a data.table")

  if(dim(coord)[2] == 2)
  {
    data.table::setnames(coord, c("x", "y"))
    data.table::setorderv(coord, c("x", "y"))
  }
  else if(dim(coord)[2] == 3)
  {
    data.table::setnames(coord, c("x", "y", "z"))
    data.table::setorderv(coord, c("x", "y", "z"))
  }
  else if(dim(coord)[2] == 4)
  {
    data.table::setnames(coord, c("x", "y", "z", "r"))
  }
  else
    stop("Dimension incorrect for 'coord'")

  if(geometry == "circle")
    return(lasclipCircle(.las, coord$x[1], coord$y[1], coord$z[1], inside))
  else if(geometry == "rectangle")
    return(lasclipRectangle(.las, coord$x[1], coord$y[1], coord$x[2], coord$y[2], inside))
  else if(geometry == "polygon")
    return(lasclipPolygon(.las, coord$x, coord$y, inside))
  else if(geometry == "cuboid")
    return(lasclipCuboid(.las, coord$x[1], coord$y[1], coord$z[1], coord$x[2], coord$y[2], coord$z[2], inside))
  else if(geometry == "sphere")
    return(lasclipSphere(.las, coord$x[1], coord$y[1], coord$z[1], coord$r[1], inside))
  else
    stop("Geometry no supported.")
}


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

lasclipCuboid = function(.las, xleft, ybottom, zbottom, xright, ytop, ztop, inside = TRUE)
{
  X <- Y <- Z <- NULL

  if(inside)
    return(lasfilter(.las, between(X, xleft, xright), between(Y, ybottom, ytop), between(Z, zbottom, ztop)))
  else
    return(lasfilter(.las, !between(X, xleft, xright), !between(Y, ybottom, ytop), !between(Z, zbottom, ztop)))
}


lasclipSphere = function(.las, xcenter, ycenter, zcenter, radius, inside = TRUE)
{
  X <- Y <- Z <- NULL

  if(inside)
    return(lasfilter(.las, (X-xcenter)^2 + (Y-ycenter)^2 + (Z - zcenter)^2 <= radius^2))
  else
    return(lasfilter(.las, (X-xcenter)^2 + (Y-ycenter)^2 + (Z - zcenter)^2 > radius^2))
}

