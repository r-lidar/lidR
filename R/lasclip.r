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
#' (\code{LAScatalog} object). With a \code{LAS} object, the user first reads and loads a point-cloud
#' in memory and then can clip it to get a subset  within a region of interest (ROI). With a \code{LAScatalog}
#' object, the user can extracts any arbitrary ROI for a set of \code{las/laz} file loading only the
#' points of interest. This is faster, easier and much more memory-efficient for extracting ROIs.
#'
#' @section Supported geometries:
#' \itemize{
#'  \item \href{https://en.wikipedia.org/wiki/Well-known_text}{WKT string}: describing a POLYGON or
#'  a MULTIPOLYGON.
#'  \item \link[sp:Polygon-class]{Polygon}
#'  \item \link[sp:Polygons-class]{Polygons}
#'  \item \link[sp:SpatialPolygons-class]{SpatialPolygons}
#'  \item \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}
#'  \item \link[sf:sf]{SimpleFeature}
#'  \item \link[raster:Extent-class]{Extent}
#'  \item \link[base:matrix]{matrix} 2 x 2 describing a bounding box following this order:
#'  \preformatted{
#'   min     max
#' x 684816  684943
#' y 5017823 5017957}
#'  \item Any other object that have a bouding box accessible via \code{raster::extent} such as a
#'  \link[raster:RasterLayer-class]{RasterLayer} or a \code{Spatial*} object. A rectangle is extracted.
#'  }
#'
#' @template LAScatalog
#'
#' @section Supported processing options for a LAScatalog:
#' \itemize{
#' \item \strong{cores}: If several ROIs are requested, they can be extracted in parallel. See \link{LAScatalog-class}.
#' \item \strong{progress}: See \link{LAScatalog-class}.
#' \item \strong{stop_early}: See \link{LAScatalog-class}.
#' \item \strong{output_files}: If 'output_files' is set in the catalog, the ROIs will not be returned in R.
#' They will be written in one or several files. See \link{LAScatalog-class} and examples. The allowed
#' templates in \code{lasclip} are \code{{XLEFT}, {XRIGHT}, {YBOTTOM}, {YTOP}, {ID}, {XCENTER},
#' {YCENTER}} or any names from the table of attributes of geospatial objects given as input such as
#' \code{{LAKENAME}} or \code{{YEAR}} for example if these fields exist. If empty everything is returned
#' into R.
#' \item \strong{laz_compression}: The ouptut files are las or laz. See also \link{LAScatalog-class}.
#' }
#'
#' @template param-las
#' @param geometry a geometric object. Many types are supported, see section 'supported geometries'.
#' @param xleft numeric. left x coordinates of rectangles.
#' @param ybottom	numeric. bottom y coordinates of rectangles.
#' @param xright numeric. right x coordinates of rectangles.
#' @param ytop numeric. top y coordinates of rectangles.
#' @param xpoly numeric. x coordinates of a polygon.
#' @param ypoly numeric. y coordinates of a polygon.
#' @param xcenter numeric. x coordinates of discs centers.
#' @param ycenter numeric. y coordinates of discs centers.
#' @param radius numeric. disc radiuses.
#'
#' @return An object of class \code{LAS} or a \code{list} of \code{LAS} objects if the query implies to return
#' several regions of interest or \code{NULL} if the query is outside the dataset or within a region that does
#' not contains any point or a \code{LAScatalog} if the query is immediatly written into file without
#' loading anything in R..
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' # Load the file and clip the region of interest
#' las = readLAS(LASfile)
#' subset1 = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
#'
#' # Do not load the file(s), extract only the region of interest from a bigger dataset
#' ctg = catalog(LASfile)
#' subset2 = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)
#'
#' # Extract all the polygons from a shapefile
#' shapefile_dir <- system.file("extdata", package = "lidR")
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#' subset3 = lasclip(ctg, lakes)
#'
#' # Extract the polygons, write them in files name after the lake names, do not load anything in R
#' output_files(ctg) <- paste0(tempfile(), "_{LAKENAME_1}")
#' lasclip(ctg, lakes)
#'
#' \dontrun{
#' plot(subset1)
#' plot(subset2)
#' plot(subset3)
#' }
#' @name lasclip
#' @export
lasclip = function(las, geometry)
{
  if (is.character(geometry))
    geometry = rgeos::readWKT(geometry)

  if (is(geometry, "Polygon"))
    geometry <- sp::Polygons(list(geometry), ID = 1)

  if (is(geometry, "Polygons"))
    geometry <- sp::SpatialPolygons(list(geometry))

  if (is(geometry, "SpatialPolygons") | is(geometry, "SpatialPolygonsDataFrame"))
    geometry <- sf::st_as_sf(geometry)

  if (is(geometry, "sf"))
  {
    if (!all(sf::st_is(geometry, "POLYGON") |sf::st_is(geometry, "MULTIPOLYGON")))
      stop("Incorrect geometry type. POLYGON and MULTIPOLYGON are supported.", call. = FALSE)

    return(lasclipSimpleFeature(las, geometry))
  }
  else if (is(geometry, "Extent"))
  {
    xmin = geometry@xmin
    xmax = geometry@xmax
    ymin = geometry@ymin
    ymax = geometry@ymax
    return(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  }
  else if (is.matrix(geometry))
  {
    if (!all(dim(geometry) == 2))
      stop("Matrix must have a size 2 x 2", call. = FALSE)

    xmin = geometry[1]
    xmax = geometry[3]
    ymin = geometry[2]
    ymax = geometry[4]
    return(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  }
  else if (tryCatch(is(raster::extent(geometry), "Extent"), error = function(e) return(FALSE)))
  {
    geometry = raster::extent(geometry)
    xmin = geometry@xmin
    xmax = geometry@xmax
    ymin = geometry@ymin
    ymax = geometry@ymax
    return(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  }
  else
  {
    stop(paste0("Geometry type ", paste0(class(geometry), collapse = " "), " not supported"), call. = FALSE)
  }
}

# =========
# RECTANGLE
# =========

#' @export
#' @rdname lasclip
lasclipRectangle = function(las, xleft, ybottom, xright, ytop)
{
  assertive::assert_is_numeric(xleft)
  assertive::assert_is_numeric(ybottom)
  assertive::assert_is_numeric(xright)
  assertive::assert_is_numeric(ytop)
  assertive::assert_are_same_length(xleft, ybottom)
  assertive::assert_are_same_length(xleft, xright)
  assertive::assert_are_same_length(xleft, ytop)

  UseMethod("lasclipRectangle", las)
}

#' @export
lasclipRectangle.LAS = function(las, xleft, ybottom, xright, ytop)
{
  X <- Y <- NULL

  output = vector(mode = "list", length(xleft))
  for (i in 1:length(xleft))
  {
    roi =  lasfilter(las, X >= xleft[i] & X < xright[i] & Y >= ybottom[i] & Y < ytop[i])
    if (is.null(roi))
    {
      warning(glue::glue("No point found for within disc ({xleft[i]}, {ybottom[i]}, {xright[i]}, {ytop[i]}). NULL returned."), call. = FALSE)
      output[i] = list(NULL)
    }
    else
      output[[i]] = roi
  }


  if(length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
lasclipRectangle.LAScatalog = function(las, xleft, ybottom, xright, ytop)
{
  bboxes  = mapply(raster::extent, xleft, xright, ybottom, ytop)
  output  = catalog_extract(las, bboxes, LIDRRECTANGLE)

  if(length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ========
# POLYGON
# ========

#' @export lasclipPolygon
#' @rdname lasclip
lasclipPolygon = function(las, xpoly, ypoly)
{
  assertive::assert_is_numeric(xpoly)
  assertive::assert_is_numeric(ypoly)
  assertive::assert_are_same_length(xpoly, ypoly)

  poly = sp::Polygon(cbind(xpoly, ypoly))
  return(lasclip(las, poly))
}

# ========
# CIRCLE
# ========

#' @export lasclipCircle
#' @rdname lasclip
lasclipCircle = function(las, xcenter, ycenter, radius)
{
  assertive::assert_is_numeric(xcenter)
  assertive::assert_is_numeric(ycenter)
  assertive::assert_is_numeric(radius)
  assertive::assert_are_same_length(xcenter, ycenter)

  UseMethod("lasclipCircle", las)
}

#' @export
lasclipCircle.LAS = function(las, xcenter, ycenter, radius)
{
  if (length(radius) > 1)
    assertive::assert_are_same_length(xcenter, radius)
  else
    radius = rep(radius, length(xcenter))

  X <- Y <- NULL

  output = vector(mode = "list", length(xcenter))
  for (i in 1:length(xcenter))
  {
    roi = lasfilter(las, (X-xcenter[i])^2 + (Y-ycenter[i])^2 <= radius[i]^2)
    if (is.null(roi))
    {
      warning(glue::glue("No point found for within disc ({xcenter[i]}, {ycenter[i]}, {radius[i]}). NULL returned."), call. = FALSE)
      output[i] = list(NULL)
    }
    else
      output[[i]] = roi
  }

  if(length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
#' @export
lasclipCircle.LAScatalog = function(las, xcenter, ycenter, radius)
{
  if (length(radius) > 1)
    assertive::assert_are_same_length(xcenter, radius)
  else
    radius = rep(radius, length(xcenter))

  xmin   = xcenter - radius
  xmax   = xcenter + radius
  ymin   = ycenter - radius
  ymax   = ycenter + radius
  bboxes = mapply(raster::extent, xmin, xmax, ymin, ymax)
  output = catalog_extract(las, bboxes, LIDRCIRCLE)

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ========
# WKT
# ========

lasclipSimpleFeature = function(las, sf)
{
  UseMethod("lasclipSimpleFeature", las)
}

lasclipSimpleFeature.LAS = function(las, sf)
{
  wkt <- sf::st_as_text(sf$geometry)

  output = vector(mode = "list", length(wkt))
  for (i in 1:length(wkt))
  {
    roi = lasfilter(las, C_points_in_polygon_wkt(las@data$X, las@data$Y, wkt[i]))
    if (is.null(roi))
    {
      warning(glue::glue("No point found for within {wkt[i]}. NULL returned."), call. = FALSE)
      output[i] = list(NULL)
    }
    else
      output[[i]] = roi
  }

  if(length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

lasclipSimpleFeature.LAScatalog = function(las, sf)
{
  wkt  <- sf::st_as_text(sf$geometry)

  bboxes = lapply(wkt, function(string)
  {
    spgeom = rgeos::readWKT(string)
    return(raster::extent(spgeom))
  })

  output = catalog_extract(las, bboxes, LIDRRECTANGLE, sf)

  if(length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# =============
# GENERIC QUERY
# =============

catalog_extract = function(ctg, bboxes, shape = LIDRRECTANGLE, sf = NULL)
{
  progress  <- progress(ctg)

  stopifnot(shape == LIDRRECTANGLE | shape == LIDRCIRCLE)

  if (progress) plot.LAScatalog(ctg, FALSE)

  # Define a function to be passed in cluster_apply
  extract_query = function(cluster)
  {
    if (is.null(cluster)) return(NULL)
    streamLAS(cluster, ofile = cluster@save, filter_wkt = cluster@wkt)
  }

  # Find the ROIs in the catalog and return LASclusters
  clusters <- catalog_index(ctg, bboxes, shape, 0)

  # Add some useful information in the clusters
  for (i in 1:length(clusters))
  {
    if(!is.null(clusters[[i]]))
    {
      if (!is.null(sf))
        clusters[[i]]@wkt = sf::st_as_text(sf$geometry[i])

      if (output_files(ctg) != "")
      {
        X         <-  if (!is.null(sf)) sf[1,] else list()
        X$ID      <- i
        X$XCENTER <- clusters[[i]]@center$x
        X$XCENTER <- clusters[[i]]@center$y
        X$XLEFT   <- clusters[[i]]@bbox$xmin
        X$XRIGHT  <- clusters[[i]]@bbox$xmax
        X$YBOTTOM <- clusters[[i]]@bbox$ymin
        X$YTOP    <- clusters[[i]]@bbox$ymax
        format    <- if (laz_compression(ctg)) ".laz" else ".las"
        clusters[[i]]@save <- paste0(glue::glue_data(X, output_files(ctg)), format)
      }
    }
  }

  output <- cluster_apply(clusters, extract_query, ctg@processing_options, ctg@output_options, drop_null = FALSE)

  if (ctg@output_options$output_files != "")   # output should contains nothing because everything have been streamed into files
  {
    written_path = c()
    for (cluster in clusters)
    {
      if (file.exists(cluster@save))
        written_path = append(written_path, cluster@save)
    }

    return(list(catalog(written_path)))
  }
  else                                          # output should contains LAS objects returned at the R level
  {
    # Transfer CRS
    for (i in 1:length(output))
    {
      if (!is.null(output[[i]]))
      {
        output[[i]]@crs <- ctg@proj4string

        # Patch to solves issue #73 waiting for a better solution in issue 2333 in data.table
        if (cores(ctg) > 1) output[[i]]@data <- data.table::alloc.col(output[[i]]@data)
      }
    }

    return(output)
  }
}