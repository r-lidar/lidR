# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2017 Jean-Romain Roussel
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
#' @param x An object of class \code{LAS}
#' @param geometry characters. name of a geometry. Can be \code{"circle"}, \code{"rectangle"},
#' \code{"polygon"}
#' @param coord matrix or data.frame. The coordinates of the minimum points required to fully
#' describe the geometry. For circle a 1-by-3 matrix, for rectangle a 2-by-2 matrix, for polygon n-by-2
#' matrix, for cuboid 2-by-3 matrix and for sphere a 1-by-4 matrix
#' @param xleft	scalar. of left x position.
#' @param ybottom	scalar. of bottom y position.
#' @param xright scalar. of right x position.
#' @param ytop scalar. of top y position.
#' @param x	numerical array. x-coordinates of polygon
#' @param y	numerical array. y-coordinates of polygon
#' @param xcenter	scalar. x disc center
#' @param ycenter	scalar. y disc center
#' @param radius scalar. Disc radius
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
#' @export
lasclip = function(x, geometry, ofile = "")
{
  UseMethod("lasclip", x)
}

#' @export
lasclip.LAS = function(x, geometry, ofile = "")
{
  if (is(geometry, "Polygon"))
  {
     las = lasclipPolygon(x, geomtry@coords[,1], geomtry@coords[,2])
     return(las)
  }
  else
  {
    stop("Geometry not supported", call. = FALSE)
  }
}

#' @export
lasclip.LAScatalog = function(x, geometry, ofile = "")
{
  if (is(geometry, "Polygon"))
  {
    las = lasclipPolygon(x, geomtry@coords[,1], geomtry@coords[,2], ofile = "")
    return(las)
  }
  else
  {
    stop("Geometry not supported", call. = FALSE)
  }
}

# ========
# POLYGON
# ========

#' @export lasclipRectangle
#' @rdname lasclip
lasclipRectangle = function(x, xleft, ybottom, xright, ytop, ofile = "")
{
  UseMethod("lasclipRectangle", x)
}

#' @export
lasclipRectangle.LAS = function(x, xleft, ybottom, xright, ytop)
{
  X <- Y <- NULL
  return(lasfilter(x, between(X, xleft, xright), between(Y, ybottom, ytop)))
}

#' @export
lasclipRectangle.LAScatalog = function(x, xleft, ybottom, xright, ytop)
{
  return(catalog_query_rect(x, xleft, xright, ybottom, ytop))
}

# ========
# POLYGON
# ========

#' @export lasclipPolygon
#' @rdname lasclip
lasclipPolygon = function(x, xpoly, ypoly, ofile = "")
{
  UseMethod("lasclipPolygon", x)
}

lasclipPolygon.LAS = function(x, xpoly, ypoly, ofile = "")
{
  X <- Y <- NULL
  return(lasfilter(x, points_in_polygon(xpoly,ypoly, X, Y)))
}

lasclipPolygon.LAScatalog = function(x, xpoly, ypoly, ofile = "")
{
  return(catalog_query_poly(x, xpoly, ypoly, ofile))
}

# ========
# CIRCLE
# ========


#' @export lasclipCircle
#' @rdname lasclip
lasclipCircle = function(x, xcenter, ycenter, radius, ofile = "")
{
  UseMethod("lasclipCircle", x)
}

#' @export
lasclipCircle.LAS = function(x, xcenter, ycenter, radius, ofile = "")
{
  X <- Y <- NULL
  return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
}

#' @export
#' @export
lasclipCircle.LAScatalog = function(x, xcenter, ycenter, radius, ofile = "")
{
  return(catalog_clip_circle(x, xcenter, ycenter, radius, ofile))
}



# lasclipCuboid = function(x, xleft, ybottom, zbottom, xright, ytop, ztop)
# {
#   X <- Y <- Z <- NULL
#   return(lasfilter(x, between(X, xleft, xright), between(Y, ybottom, ytop), between(Z, zbottom, ztop)))
# }
#
#
# lasclipSphere = function(x, xcenter, ycenter, zcenter, radius)
# {
#   X <- Y <- Z <- NULL
#   return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 + (Z - zcenter)^2 <= radius^2))
# }

