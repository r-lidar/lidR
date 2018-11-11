# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel
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
#' Clip LiDAR points within a given geometry from a point cloud (\code{LAS} object) or a catalog
#' (\code{LAScatalog} object)
#'
#' \code{lasclip} functions work both on \code{LAS} and \code{LAScatalog} objects. With a \code{LAS}
#' object, the user first reads and loads a point-cloud and then clip it to get a subset within a region
#' of interest (ROI). With a \code{LAScatalog} object, the user extracts the ROI without
#' loading the whole point-cloud. This is faster and much more memory-efficient for extracting ROIs.
#' \cr\cr
#' Minor differences exist between the \code{LAS} and \code{LAScatalog} version of \code{lasclip}.
#' For example the user can clip a \code{SpatialPolygonsDataFrame} from a \code{LAS} object but not
#' from a \code{LAScatalog}. Also the option \code{inside = FALSE} is disabled for \code{LAScatalog}
#' objects. These differences are generally justified by memory safety concerns.
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
#' file while reading. This is much more memory-efficient than loading into R first, then writing.
#' @param inside logical. Inverts the selection (only with a \code{LAS} object). Select inside or outside
#' the shape.
#' @param ... Additional argument for readLAS to reduce the amount of data loaded (only with a
#' \code{LAScatalog} object)
#' @return An object of class \code{LAS} or NULL if the result is immediately written to a file.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' # Load the file and clip the region of interest
#' las = readLAS(LASfile)
#' subset1 = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
#'
#' # Do not load the file, extract only the region of interest
#' ctg = catalog(LASfile)
#' subset2 = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)
#'
#' # Extract a polygon from a shapefile
#' shapefile_dir <- system.file("extdata", package = "lidR")
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#' lake = lakes@polygons[[1]]@Polygons[[1]]
#' subset3 = lasclip(ctg, lake)
#'
#' # Extract a polygon, write it in a file, do not load anything in R
#' file = paste0(tempfile(), ".las")
#' lasclip(ctg, lake, ofile = file)
#'
#' \dontrun{
#' plot(subset1)
#' plot(subset2)
#' plot(subset3)
#' }
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
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  if (is(geometry, "Polygon"))
  {
     las = lasclipPolygon(x, geometry@coords[,1], geometry@coords[,2], inside = inside)
     return(las)
  }
  else if (is(geometry, "SpatialPolygonsDataFrame"))
  {
    id = classify_from_shapefile(x, geometry)
    X = split(x@data, id)
    X = lapply(X, LAS, header = x@header)
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
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  if (is(geometry, "Polygon"))
    return(lasclipPolygon(x, geometry@coords[,1], geometry@coords[,2], ofile, inside))
  else if (is(geometry, "SpatialPolygonsDataFrame"))
    stop("'SpatialPolygonsDataFrame' is not supported for 'LAScatalog' objects", call. = FALSE)
  else
    stop("Geometry not supported", call. = FALSE)
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
  assertive::assert_is_numeric(xleft)
  assertive::assert_is_numeric(ybottom)
  assertive::assert_is_numeric(xright)
  assertive::assert_is_numeric(ytop)
  assertive::assert_are_same_length(xleft, ybottom)
  assertive::assert_are_same_length(xleft, xright)
  assertive::assert_are_same_length(xleft, ytop)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  X <- Y <- NULL

  if (length(xleft) == 1)
  {
    if (inside)
      return(lasfilter(x, between(X, xleft, xright), between(Y, ybottom, ytop)))
    else
      return(lasfilter(x, !(between(X, xleft, xright) & between(Y, ybottom, ytop))))
  }
  else
  {
    output = vector(mode = "list", length(xleft))
    for (i in 1:length(xleft))
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
  assertive::assert_is_numeric(xleft)
  assertive::assert_is_numeric(ybottom)
  assertive::assert_is_numeric(xright)
  assertive::assert_is_numeric(ytop)
  assertive::assert_are_same_length(xleft, ybottom)
  assertive::assert_are_same_length(xleft, xright)
  assertive::assert_are_same_length(xleft, ytop)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  if (!inside) stop("'inside = FALSE' is not supported for 'LAScatalog' objects.")

  if (length(xleft) == 1)
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
  assertive::assert_is_numeric(xpoly)
  assertive::assert_is_numeric(ypoly)
  assertive::assert_are_same_length(xpoly, ypoly)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  X <- Y <- NULL

  if( inside)
    return(lasfilter(x, C_points_in_polygon(xpoly,ypoly, X, Y)))
  else
    return(lasfilter(x, !C_points_in_polygon(xpoly,ypoly, X, Y)))
}

#' @export
lasclipPolygon.LAScatalog = function(x, xpoly, ypoly, ofile = "", inside = TRUE, ...)
{
  assertive::assert_is_numeric(xpoly)
  assertive::assert_is_numeric(ypoly)
  assertive::assert_are_same_length(xpoly, ypoly)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  if (!inside) stop("'inside = FALSE' is not supported for 'LAScatalog' objects.")

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
  assertive::assert_is_numeric(xcenter)
  assertive::assert_is_numeric(ycenter)
  assertive::assert_is_numeric(radius)
  assertive::assert_are_same_length(xcenter, ycenter)
  if (length(radius) > 1) assertive::assert_are_same_length(xcenter, radius)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  X <- Y <- NULL

  if (length(xcenter) == 1)
  {
    if (inside)
      return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
    else
      return(lasfilter(x, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
  }
  else
  {
    output = vector(mode = "list", length(xcenter))
    for (i in 1:length(xcenter))
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
  assertive::assert_is_numeric(xcenter)
  assertive::assert_is_numeric(ycenter)
  assertive::assert_is_numeric(radius)
  assertive::assert_are_same_length(xcenter, ycenter)
  if (length(radius) > 1) assertive::assert_are_same_length(xcenter, radius)
  assertive::assert_is_a_string(ofile)
  assertive::assert_is_a_bool(inside)

  if (!inside) stop("'inside = FALSE' is not available for 'LAScatalog' objects.")

  if (length(xcenter) == 1)
    return(catalog_clip_circ(x, xcenter, ycenter, radius, ofile, ...))
  else
    return(catalog_queries(x, xcenter, ycenter, radius, ...))
}


catalog_clip_poly = function(catalog, xpoly, ypoly, ofile, ...)
{
  xmin <- min(xpoly)
  xmax <- max(xpoly)
  ymin <- min(ypoly)
  ymax <- max(ypoly)
  xc   <- (xmax + xmin)/2
  yc   <- (ymax + ymin)/2
  w    <- xmax - xmin + 1
  h    <- ymax - ymin + 1

  cluster  <- catalog_index(catalog, xc, yc, w, h, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  p = list(...)

  filter = cluster@filter
  select = "*"

  if(!is.null(p$filter))
    filter = paste(filter, p$filter)

  if(!is.null(p$select))
    select = p$select

  header = rlas::read.lasheader(cluster@files[1])
  data   = rlas:::stream.las(cluster@files, ofile = ofile, select = select, filter = filter)

  header = rlas::read.lasheader(cluster@files[1])
  data   = rlas:::stream.las_inpoly(cluster@files, xpoly, ypoly, select = select, filter = filter, ofile = ofile)

  if (is.null(data))
    return (invisible())

  if (nrow(data) == 0)
    return (invisible())

  return(LAS(data, header))
}

catalog_clip_rect = function(catalog, xmin, ymin, xmax, ymax, ofile, ...)
{
  xc <- (xmax + xmin)/2
  yc <- (ymax + ymin)/2
  w  <- xmax - xmin
  h  <- ymax - ymin

  cluster  <- catalog_index(catalog, xc, yc, w, h, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  p = list(...)

  filter = cluster@filter
  select = "*"

  if(!is.null(p$filter))
    filter = paste(filter, p$filter)

  if(!is.null(p$select))
    select = p$select

  header = rlas::read.lasheader(cluster@files[1])
  data   = rlas:::stream.las(cluster@files, ofile = ofile, select = select, filter = filter)

  if (is.null(data))
    return (invisible())

  if (nrow(data) == 0)
    return (invisible())

  return(LAS(data, header))
}

catalog_clip_circ = function(catalog, xcenter, ycenter, radius, ofile, ...)
{
  cluster  <- catalog_index(catalog, xcenter, ycenter, 2*radius, NULL, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  p = list(...)

  filter = cluster@filter
  select = "*"

  if(!is.null(p$filter))
    filter = paste(filter, p$filter)

  if(!is.null(p$select))
    select = p$select

  header = rlas::read.lasheader(cluster@files[1])
  data   = rlas:::stream.las(cluster@files, ofile = ofile, select = select, filter = filter)

  if (is.null(data))
    return (invisible())

  if (nrow(data) == 0)
    return (invisible())

  return(LAS(data, header))
}

