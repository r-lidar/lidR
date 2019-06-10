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

# ======== GENERIC =========

#' Clip LiDAR points
#'
#' Clip LiDAR points within a given geometry from a point cloud (\code{LAS} object) or a catalog
#' (\code{LAScatalog} object). With a \code{LAS} object, the user first reads and loads a point-cloud
#' into memory and then can clip it to get a subset within a region of interest (ROI). With a \code{LAScatalog}
#' object, the user can extract any arbitrary ROI for a set of \code{las/laz} files, loading only the
#' points of interest. This is faster, easier and much more memory-efficient for extracting ROIs.
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
#' @param radius numeric. disc radius or radii.
#' @param ... optional supplementary options (see supported geometries)
#'
#' @section Supported geometries:
#' \itemize{
#'  \item \href{https://en.wikipedia.org/wiki/Well-known_text}{WKT string}: describing a POINT, a POLYGON or
#'  a MULTIPOLYGON.
#'  \item \link[sp:Polygon-class]{Polygon} or \link[sp:Polygons-class]{Polygons}
#'  \item \link[sp:SpatialPolygons-class]{SpatialPolygons} or \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}
#'  \item \link[sp:SpatialPoints-class]{SpatialPoints} or \link[sp:SpatialPointsDataFrame-class]{SpatialPointsDataFrame}
#'  in that case a parameter 'radius' must be passed in '...'
#'  \item \link[sf:sf]{SimpleFeature} that consistently contains \code{POINT} or \code{POLYGON/MULTIPOLYGON}.
#'  In case of \code{POINT} a parameter 'radius' must be passed in '...'
#'  \item \link[raster:Extent-class]{Extent}
#'  \item \link[base:matrix]{matrix} 2 x 2 describing a bounding box following this order:
#'  \preformatted{
#'   min     max
#' x 684816  684943
#' y 5017823 5017957}
#'  }
#'
#' @template LAScatalog
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item chunk_size: Does not make sense here.
#' \item buffer: Not supported yet.
#' \item alignment: Does not makes sense here.
#' \item \strong{progress}: Displays a progress estimation.
#' \item \strong{stop_early}: Leave this 'as-is' unless you are an advanced user.
#' \item \strong{output_files}: If 'output_files' is set in the catalog, the ROIs will not be returned in R.
#' They will be written immediately in files. See \link{LAScatalog-class} and examples. The allowed templates in
#' \code{lasclip} are \code{{XLEFT}, {XRIGHT}, {YBOTTOM}, {YTOP}, {ID}, {XCENTER},
#' {YCENTER}} or any names from the table of attributes of a spatial object given as
#' input such as \code{{PLOT_ID}} or \code{{YEAR}}, for example, if these attributes exist. If empty everything
#' is returned into R.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files
#' \item \strong{drivers}: Leave this 'as-is' unless you are an advanced user.
#' \item select: The function will write files equivalent to the originals. This option is not respected.
#' \item \strong{filter}: Read only the points of interest.
#' }
#'
#' @return If the input is a \code{LAS} object: an object of class \code{LAS}, or a \code{list} of \code{LAS} objects if the query
#' implies several regions of interest will be returned.\cr\cr
#' If the input is a \code{LAScatalog} object: an object of class \code{LAS}, or a \code{list} of \code{LAS} objects if the query
#' implies several regions of interest will be returned, or a \code{LAScatalog} if the queries are immediately written into files
#' without loading anything in R.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' # Load the file and clip the region of interest
#' las = readLAS(LASfile)
#' subset1 = lasclipRectangle(las, 684850, 5017850, 684900, 5017900)
#'
#' # Do not load the file(s), extract only the region of interest from a bigger dataset
#' ctg = readLAScatalog(LASfile)
#' subset2 = lasclipRectangle(ctg, 684850, 5017850, 684900, 5017900)
#'
#' # Extract all the polygons from a shapefile
#' shapefile_dir <- system.file("extdata", package = "lidR")
#' lakes = shapefile(paste0(shapefile_dir, "/lake_polygons_UTM17.shp"))
#' subset3 = lasclip(ctg, lakes)
#'
#' # Extract the polygons, write them in files named after the lake names, do not load anything in R
#' opt_output_files(ctg) <- paste0(tempfile(), "_{LAKENAME_1}")
#' new_ctg = lasclip(ctg, lakes)
#'
#' \dontrun{
#' plot(subset1)
#' plot(subset2)
#' plot(subset3)
#' }
#' @name lasclip
#' @export
lasclip = function(las, geometry, ...)
{
  if (is.character(geometry))
    geometry <- rgeos::readWKT(geometry, p4s = las@proj4string)

  if (is(geometry, "Polygon"))
    geometry <- sp::Polygons(list(geometry), ID = 1)

  if (is(geometry, "Polygons"))
    geometry <- sp::SpatialPolygons(list(geometry), proj4string = las@proj4string)

  if (is(geometry, "SpatialPolygons") | is(geometry, "SpatialPolygonsDataFrame"))
    geometry <- sf::st_as_sf(geometry)

  if (is(geometry, "SpatialPoints") | is(geometry, "SpatialPointsDataFrame"))
    geometry <- sf::st_as_sf(geometry)

  if (is(geometry, "sf"))
  {
    if (all(sf::st_is(geometry, "POLYGON") | sf::st_is(geometry, "MULTIPOLYGON")))
    {
      return(lasclipSimpleFeature(las, geometry))
    }
    else if (all(sf::st_is(geometry, "POINT")))
    {
      p <- list(...)
      if (is.null(p$radius))
        stop("Clipping using sfc_POINT or SpatialPoints* requires addition of parameter 'radius'.")

      centers <- sf::st_coordinates(geometry)
      ycenter <- centers[,2]
      xcenter <- centers[,1]
      radius  <- p$radius
      bboxes  <- mapply(raster::extent, xcenter - radius, xcenter + radius, ycenter - radius, ycenter + radius)
      output  <- catalog_extract(las, bboxes, LIDRCIRCLE, data = geometry)

      if (length(output) == 0)
        return(NULL)
      else if (length(output) == 1)
        return(output[[1]])
      else
        return(output)
    }
    else
      stop("Incorrect geometry type. POINT, POLYGON and MULTIPOLYGON are supported.")
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
      stop("Matrix must have a size 2 x 2")

    xmin = geometry[1]
    xmax = geometry[3]
    ymin = geometry[2]
    ymax = geometry[4]
    return(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  }
  #else if (tryCatch(is(raster::extent(geometry), "Extent"), error = function(e) return(FALSE)))
  #{
    #geometry = raster::extent(geometry)
    #xmin = geometry@xmin
    #xmax = geometry@xmax
    #ymin = geometry@ymin
    #ymax = geometry@ymax
    #return(lasclipRectangle(las, xmin, ymin, xmax, ymax))
  #}
  else
  {
    stop(paste0("Geometry type ", paste0(class(geometry), collapse = " "), " not supported"))
  }
}

# ========= RECTANGLE =========

#' @export
#' @rdname lasclip
lasclipRectangle = function(las, xleft, ybottom, xright, ytop)
{
  assert_is_numeric(xleft)
  assert_is_numeric(ybottom)
  assert_is_numeric(xright)
  assert_is_numeric(ytop)
  assert_are_same_length(xleft, ybottom)
  assert_are_same_length(xleft, xright)
  assert_are_same_length(xleft, ytop)

  UseMethod("lasclipRectangle", las)
}

#' @export
lasclipRectangle.LAS = function(las, xleft, ybottom, xright, ytop)
{
  X <- Y <- NULL

  output <- vector(mode = "list", length(xleft))
  for (i in 1:length(xleft))
  {
    roi <- lasfilter(las, X >= xleft[i] & X < xright[i] & Y >= ybottom[i] & Y < ytop[i])
    if (is.empty(roi)) warning(glue::glue("No point found for within disc ({xleft[i]}, {ybottom[i]}, {xright[i]}, {ytop[i]})."))
    output[[i]] = roi
  }

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
lasclipRectangle.LAScatalog = function(las, xleft, ybottom, xright, ytop)
{
  bboxes  <- mapply(raster::extent, xleft, xright, ybottom, ytop)
  output  <- catalog_extract(las, bboxes, LIDRRECTANGLE)

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ========  POLYGON ========

#' @export lasclipPolygon
#' @rdname lasclip
lasclipPolygon = function(las, xpoly, ypoly)
{
  assert_is_numeric(xpoly)
  assert_is_numeric(ypoly)
  assert_are_same_length(xpoly, ypoly)

  poly <- sp::Polygon(cbind(xpoly, ypoly))
  return(lasclip(las, poly))
}

# ======== CIRCLE ========

#' @export lasclipCircle
#' @rdname lasclip
lasclipCircle = function(las, xcenter, ycenter, radius)
{
  assert_is_numeric(xcenter)
  assert_is_numeric(ycenter)
  assert_is_numeric(radius)
  assert_are_same_length(xcenter, ycenter)
  UseMethod("lasclipCircle", las)
}

#' @export
lasclipCircle.LAS = function(las, xcenter, ycenter, radius)
{
  if (length(radius) > 1)
    assert_are_same_length(xcenter, radius)
  else
    radius <- rep(radius, length(xcenter))

  X <- Y <- NULL

  output <- vector(mode = "list", length(xcenter))
  for (i in 1:length(xcenter))
  {
    roi <- lasfilter(las, (X - xcenter[i])^2 + (Y - ycenter[i])^2 <= radius[i]^2)
    if (is.empty(roi)) warning(glue::glue("No point found for within disc ({xcenter[i]}, {ycenter[i]}, {radius[i]})."))
    output[[i]] <- roi
  }

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
lasclipCircle.LAScatalog = function(las, xcenter, ycenter, radius)
{
  if (length(radius) > 1)
    assert_are_same_length(xcenter, radius)
  else
    radius <- rep(radius, length(xcenter))

  xmin   <- xcenter - radius
  xmax   <- xcenter + radius
  ymin   <- ycenter - radius
  ymax   <- ycenter + radius
  bboxes <- mapply(raster::extent, xmin, xmax, ymin, ymax)
  output <- catalog_extract(las, bboxes, LIDRCIRCLE)

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ======== WKT ========

lasclipSimpleFeature = function(las, sf)
{
  UseMethod("lasclipSimpleFeature", las)
}

lasclipSimpleFeature.LAS = function(las, sf)
{
  wkt <- sf::st_as_text(sf$geometry, digits = 10)

  output = vector(mode = "list", length(wkt))
  for (i in 1:length(wkt))
  {
    roi = lasfilter(las, C_points_in_polygon_wkt(las@data$X, las@data$Y, wkt[i], getThread()))
    if (is.empty(roi)) warning(glue::glue("No point found for within {wkt[i]}."))
    output[[i]] = roi
  }

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

lasclipSimpleFeature.LAScatalog = function(las, sf)
{
  wkt  <- sf::st_as_text(sf$geometry, digits = 10)

  bboxes <- lapply(wkt, function(string)
  {
    spgeom <- rgeos::readWKT(string)
    return(raster::extent(spgeom))
  })

  output = catalog_extract(las, bboxes, LIDRRECTANGLE, sf = sf)

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ============= GENERIC QUERY  =============

catalog_extract = function(ctg, bboxes, shape = LIDRRECTANGLE, sf = NULL, data = NULL)
{
  stopifnot(shape == LIDRRECTANGLE | shape == LIDRCIRCLE)

  if (opt_progress(ctg)) plot.LAScatalog(ctg, mapview = FALSE)

  # Define a function to be passed in cluster_apply
  extract_query = function(cluster)
  {
    if (cluster@files[1] == "")
      return(NULL)

    x <- suppressMessages(suppressWarnings(streamLAS(cluster, ofile = cluster@save, filter_wkt = cluster@wkt)))

    if (is.null(x))
    {
      x <- 0
      class(x) <- "lidr_internal_skip_write"
    }

    return(x)
  }

  # Find the ROIs in the catalog and return LASclusters. If a ROI fall outside the catalog
  # its associated LAScluster is NULL and must receive a special treatment in following code
  clusters <- catalog_index(ctg, bboxes, shape, 0, outside_catalog_is_null = FALSE)

  # Add some information in the clusters to correctly extract polygons and to write correct file names
  for (i in 1:length(clusters))
  {
    # skip NULL clusters
    if (clusters[[i]]@files[1] == "")
      next

    # If a simple feature is provided we want to extract a polygon. Insert WKT string
    if (!is.null(sf))
      clusters[[i]]@wkt = sf::st_as_text(sf$geometry[i], digits = 10)

    # If the user wants to write the ROIs in files. Generate a filename.
    if (opt_output_files(ctg) != "")
    {
      if (!is.null(sf))
      {
        if (ncol(sf) > 1)
        {
          X <- sf[i,]
        }
        else
        {
          X <- sf[i,]
          X <- as.list(X)
          names(X) <- names(sf)
        }
      }
      else if (!is.null(data))
      {
        if (ncol(data) > 1)
        {
          X <- data[i,]
        }
        else
        {
          X <- data[i,]
          X <- as.list(X)
          names(X) <- names(data)
        }
      }
      else
        X <- list()

      X$ID      <- i
      X$XCENTER <- format(clusters[[i]]@center$x, scientific = F)
      X$XCENTER <- format(clusters[[i]]@center$y, scientific = F)
      X$XLEFT   <- format(clusters[[i]]@bbox[1], scientific = F)
      X$XRIGHT  <- format(clusters[[i]]@bbox[3], scientific = F)
      X$YBOTTOM <- format(clusters[[i]]@bbox[2], scientific = F)
      X$YTOP    <- format(clusters[[i]]@bbox[4], scientific = F)
      format    <- if (opt_laz_compression(ctg)) ".laz" else ".las"

      filepath  <- paste0(glue::glue_data(X, opt_output_files(ctg)), format)
      n         <- length(filepath)

      if (n > 1)
        stop(glue::glue("Ill-formed template string in the catalog: {n} filenames were generate for each region of interest"))

      clusters[[i]]@save <- filepath
    }
  }

  # Process the cluster using LAScatalog internal engine
  output <- cluster_apply(clusters, extract_query, ctg@processing_options, ctg@output_options)

  # output should contain nothing because everything has been streamed into files
  if (opt_output_files(ctg) != "")
  {
    written_path = c()
    for (i in seq_along(clusters))
    {
      if (clusters[[i]]@files[1] == "")
      {
        message(glue::glue("No point found for within region of interest {i}."))
        next
      }

      if (file.exists(clusters[[i]]@save))
        written_path = append(written_path, clusters[[i]]@save)
      else
        message(glue::glue("No point found for within region of interest {i}."))
    }

    new_ctg <- suppressMessages(readLAScatalog(written_path))
    opt_copy(new_ctg) <- ctg
    return(list(new_ctg))
  }
  # output should contain LAS objects returned at the R level
  else
  {
    for (i in 1:length(output))
    {
      if (!is.null(output[[i]]))
      {
        # Transfer the CRS of the catalog.
        output[[i]]@proj4string <- ctg@proj4string
      }
      else
      {
        # For consistency with LAS dispatched functions, LAScatalog must return empty LAS objects that respect
        # the select option. The following is definitively a twist to get a consistent output but happened
        # only for dummy queries outide the catalog
        emptylas <- readLAS(ctg@data$filename[1], ctg@input_options$select, filter = "-inside 0 0 0 0")
        output[[i]] <- emptylas
        message(glue::glue("No point found for within region of interest {i}."))
      }
    }

    return(output)
  }
}
