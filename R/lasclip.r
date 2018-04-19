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
#' Clip LiDAR points within a given geometry and convenient wrappers for most common geometries.
#'
#' @param x An object of class \code{LAS} or \code{LAScatalog}.
#' @param geometry a geometric object. Currently \code{Polygon} and \code{SpatialPolygonsDataFrame}
#' from \code{sp} are supported.
#' @param xleft scalar of left x position of rectangle.
#' @param ybottom	scalar of bottom y position of rectangle.
#' @param xright scalar of right x position of rectangle.
#' @param ytop scalar of top y position of rectangle.
#' @param xpoly numerical array. x-coordinates of polygon.
#' @param ypoly numerical array. y-coordinates of polygon.
#' @param xcenter scalar of x disc center.
#' @param ycenter scalar of y disc center.
#' @param radius scalar of disc radius.
#' @param ofile character. Path to an output file (only with a \code{LAScatalog} object).
#' If \code{ofile = ""} the result is loaded into R, otherwise the result is written to a
#' file while reading. This is much more memory efficient than loading into R first, then writing.
#' @param inside logical. Invert the selection (only with a \code{LAS} object). Select inside or outside
#' the shape.
#' @param ... Additionnal argument for readLAS to reduce the number of data loaded (only with a
#' \code{LAScatalog} object)
#' @return An object of class \code{LAS} or NULL if the result is immediately written to a file.
#' @examples
#' # Load the file and clip
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
#' plot(subset)
#' @name lasclip
#' @export
#' @export
lasclip = function(x, geometry, ofile = "", inside = TRUE, ...)
{
  UseMethod("lasclip", x)
}

#' @export
lasclip.LAS = function(x, geometry, ofile = "", inside = TRUE, ...)
{
  if (is(geometry, "Polygon"))
  {
     las = lasclipPolygon(x, geometry@coords[,1], geometry@coords[,2], inside = inside)
     return(las)
  }
  else if (is(geometry, "SpatialPolygonsDataFrame"))
  {
    id = classify_from_shapefile(x, geometry)
    X = split(x@data, id)
    X = lapply(X, LAS, header = las@header)
    return(X)
  }
  else
  {
    stop("Geometry not supported", call. = FALSE)
  }
}

#' @export
lasclip.LAScatalog = function(x, geometry, ofile = "", inside = TRUE, ...)
{
  if (is(geometry, "Polygon"))
  {
    las = lasclipPolygon(x, geometry@coords[,1], geometry@coords[,2], ofile, inside)
    return(las)
  }
  else
  {
    stop("Geometry not supported", call. = FALSE)
  }
}

# =========
# RECTANGLE
# =========

#' @export
#' @rdname lasclip
lasclipRectangle = function(x, xleft, ybottom, xright, ytop, ofile = "", inside = TRUE, ...)
{
  UseMethod("lasclipRectangle", x)
}

#' @export
lasclipRectangle.LAS = function(x, xleft, ybottom, xright, ytop, ofile = "", inside = TRUE, ...)
{
  X <- Y <- NULL

  l1 = length(xleft)
  l2 = length(ybottom)
  l3 = length(xright)
  l4 = length(ytop)

  stopifnot(is.character(ofile), is.logical(inside))

  if (l1 != l2 | l1 != l3 | l1 != l4)
    stop("Different input lenghts.")

  if (l1 == 1)
  {
    if (inside)
      return(lasfilter(x, between(X, xleft, xright), between(Y, ybottom, ytop)))
    else
      return(lasfilter(x, !(between(X, xleft, xright) & between(Y, ybottom, ytop))))
  }
  else
  {
    output = vector(mode = "list", l1)
    for (i in 1:l1)
    {
      if (inside)
        output[[i]] = lasfilter(x, between(X, xleft[i], xright[i]), between(Y, ybottom[i], ytop[i]))
      else
        output[[i]] = lasfilter(x, !(between(X, xleft[i], xright[i]) & between(Y, ybottom[i], ytop[i])))
    }

    return(output)
  }
}

#' @export
lasclipRectangle.LAScatalog = function(x, xleft, ybottom, xright, ytop, ofile = "", inside = TRUE, ...)
{
  if (!inside)
    stop("'inside = FALSE' is not available for 'LAScatalog' objects.")

  l1 = length(xleft)
  l2 = length(ybottom)
  l3 = length(xright)
  l4 = length(ytop)

  stopifnot(is.character(ofile))

  if (l1 != l2 | l1 != l3 | l1 != l4)
    stop("Different input lenghts.")

  if (l1 == 1)
  {
    return(catalog_clip_rect(x, xleft, ybottom, xright, ytop, ofile, ...))
  }
  else
  {
    xcenter = (xleft + xright)/2
    ycenter = (ybottom + ytop)/2
    width   = (xright - xleft)/2
    height  = (ytop - ybottom)/2
    return(catalog_queries(x, xcenter, ycenter, width, height, ...))
  }
}

# ========
# POLYGON
# ========

#' @export lasclipPolygon
#' @rdname lasclip
lasclipPolygon = function(x, xpoly, ypoly, ofile = "", inside = TRUE, ...)
{
  UseMethod("lasclipPolygon", x)
}

#' @export
lasclipPolygon.LAS = function(x, xpoly, ypoly, ofile = "", inside = TRUE, ...)
{
  X <- Y <- NULL

  l1 = length(xpoly)
  l2 = length(ypoly)

  stopifnot(is.logical(inside))

  if (l1 != l2)
    stop("Different input lenghts.")

  if( inside)
    return(lasfilter(x, C_points_in_polygon(xpoly,ypoly, X, Y)))
  else
    return(lasfilter(x, !C_points_in_polygon(xpoly,ypoly, X, Y)))
}

#' @export
lasclipPolygon.LAScatalog = function(x, xpoly, ypoly, ofile = "", inside = TRUE, ...)
{
  if (!inside)
    stop("'inside = FALSE' is not available for 'LAScatalog' objects.")

  return(catalog_clip_poly(x, xpoly, ypoly, ofile, ...))
}

# ========
# CIRCLE
# ========

#' @export lasclipCircle
#' @rdname lasclip
lasclipCircle = function(x, xcenter, ycenter, radius, ofile = "", inside = TRUE, ...)
{
  UseMethod("lasclipCircle", x)
}

#' @export
lasclipCircle.LAS = function(x, xcenter, ycenter, radius, ofile = "", inside = TRUE, ...)
{
  X <- Y <- NULL

  l1 = length(xcenter)
  l2 = length(ycenter)
  l3 = length(radius)

  stopifnot(is.logical(inside))

  if (l1 != l2 | l1 != l3)
    stop("Different input lenghts.")

  if (l1 == 1)
  {
    if (inside)
      return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
    else
      return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
  }
  else
  {
    output = vector(mode = "list", l1)
    for (i in 1:l1)
    {
      if (inside)
        output[[i]] = lasfilter(x, (X-xcenter[i])^2 + (Y-ycenter[i])^2 <= radius[i]^2)
      else
        output[[i]] = lasfilter(x, (X-xcenter[i])^2 + (Y-ycenter[i])^2 > radius[i]^2)
    }

    return(output)
  }
}

#' @export
#' @export
lasclipCircle.LAScatalog = function(x, xcenter, ycenter, radius, ofile = "", inside = TRUE, ...)
{
  if (!inside)
    stop("'inside = FALSE' is not available for 'LAScatalog' objects.")

  l1 = length(xcenter)
  l2 = length(ycenter)
  l3 = length(radius)

  stopifnot(is.character(ofile))

  if (l1 != l2 | l1 != l3)
    stop("Different input lenghts.")

  if (l1 == 1)
    return(catalog_clip_circ(x, xcenter, ycenter, radius, ofile, ...))
  else
    return(catalog_queries(x, xcenter, ycenter, radius, ...))
}

