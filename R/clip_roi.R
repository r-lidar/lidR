# ======== GENERIC =========

#' Clip points in regions of interest
#'
#' Clip points within a given region of interest (ROI) from a point cloud (LAS object) or a collection
#' of files (LAScatalog object).
#'
#' @template param-las
#' @param geometry a geometric object. spatial points, spatial polygons in sp or sf/sfc format, Extent,
#' bbox, 2x2 matrix
#' @param xleft,ybottom,xright,ytop numeric. coordinates of one or several rectangles.
#' @param xpoly,ypoly numeric. x coordinates of a polygon.
#' @param xcenter,ycenter numeric. x coordinates of on or several disc centres.
#' @param radius numeric. disc radius or radii.
#' @param ... in `clip_roi`: optional supplementary options (see supported geometries). Unused in
#' other functions
#'
#' @section Non-supported LAScatalog options:
#' The option `chunk size`, `buffer`, `chunk alignment` and `select` are not supported by `clip_*`
#' because they are meaningless in this context.
#'
#' @return If the input is a LAS object: an object of class LAS, or a `list` of LAS objects if the
#' query implies several regions of interest.\cr\cr
#' If the input is a LAScatalog object: an object of class LAS, or a `list` of LAS
#' objects if the query implies several regions of interest, or a LAScatalog if the
#' queries are immediately written into files without loading anything in R.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' # Load the file and clip the region of interest
#' las = readLAS(LASfile, select = "xyz", filter = "-keep_first")
#' subset1 = clip_rectangle(las, 684850, 5017850, 684900, 5017900)
#'
#' # Do not load the file(s), extract only the region of interest
#' # from a bigger dataset
#' ctg = readLAScatalog(LASfile, progress = FALSE, filter = "-keep_first")
#' subset2 = clip_rectangle(ctg, 684850, 5017850, 684900, 5017900)
#'
#' # Extract all the polygons from a shapefile
#' f <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
#' lakes <- sf::st_read(f, quiet = TRUE)
#' subset3 <- clip_roi(las, lakes)
#'
#' # Extract the polygons for a catalog, write them in files named
#' # after the lake names, do not load anything in R
#' opt_output_files(ctg) <- paste0(tempfile(), "_{LAKENAME_1}")
#' new_ctg = clip_roi(ctg, lakes)
#' plot(new_ctg)
#'
#' # Extract a transect
#' p1 <- c(684800, y = 5017800)
#' p2 <- c(684900, y = 5017900)
#' tr1 <- clip_transect(las, p1, p2, width = 4)
#'
#' \dontrun{
#' plot(subset1)
#' plot(subset2)
#' plot(subset3)
#'
#' plot(tr1, axis = TRUE, clear_artifacts = FALSE)
#' }
#' @name clip
#' @md
#' @export
clip_roi = function(las, geometry, ...)
{
  if (is.character(geometry))
    geometry <- sf::st_as_sfc(geometry, crs = sf::st_crs(las))

  if (is(geometry, "Polygon"))
    geometry <- sf::st_sfc(sf::st_polygon(list(geometry@coords)), crs = st_crs(las))

  if (inherits(geometry, "Spatial"))
    geometry <- sf::st_as_sf(geometry)

  if (is(geometry, "sfg"))
    geometry <- sf::st_sfc(geometry, crs = st_crs(las))

  if (is(geometry, "sf") | is(geometry, "sfc"))
  {
    if (all(sf::st_is(geometry, "POLYGON") | sf::st_is(geometry, "MULTIPOLYGON")))
    {
      return(clip_sf(las, geometry))
    }
    else if (all(sf::st_is(geometry, "POINT")))
    {
      p <- list(...)
      if (is.null(p$radius))
        stop("Clipping using sfc_POINT or SpatialPoints* requires addition of parameter 'radius'.", call. = FALSE)

      centers <- sf::st_coordinates(geometry)
      ycenter <- centers[,2]
      xcenter <- centers[,1]
      radius  <- p$radius
      return(clip_circle(las, xcenter, ycenter, radius, data = geometry))
    }
    else
      stop("Incorrect geometry type. POINT, POLYGON and MULTIPOLYGON are supported.", call. = FALSE)
  }
  else if (is(geometry, "Extent"))
  {
    xmin <- geometry@xmin
    xmax <- geometry@xmax
    ymin <- geometry@ymin
    ymax <- geometry@ymax
    return(clip_rectangle(las, xmin, ymin, xmax, ymax))
  }
  else if (is.matrix(geometry))
  {
    if (!all(dim(geometry) == 2))
      stop("Matrix must have a size 2 x 2")

    xmin <- geometry[1]
    xmax <- geometry[3]
    ymin <- geometry[2]
    ymax <- geometry[4]
    return(clip_rectangle(las, xmin, ymin, xmax, ymax))
  }
  else if (is(geometry, "bbox"))
  {
    xmin <- geometry$xmin
    xmax <- geometry$xmax
    ymin <- geometry$ymin
    ymax <- geometry$ymax
    return(clip_rectangle(las, xmin, ymin, xmax, ymax))
  }
  else
  {
    stop(paste0("Geometry type ", paste0(class(geometry), collapse = " "), " not supported"))
  }
}

# ========= RECTANGLE =========

#' @export
#' @rdname clip
clip_rectangle = function(las, xleft, ybottom, xright, ytop, ...)
{
  assert_is_numeric(xleft)
  assert_is_numeric(ybottom)
  assert_is_numeric(xright)
  assert_is_numeric(ytop)
  assert_are_same_length(xleft, ybottom)
  assert_are_same_length(xleft, xright)
  assert_are_same_length(xleft, ytop)

  UseMethod("clip_rectangle", las)
}

#' @export
clip_rectangle.LAS = function(las, xleft, ybottom, xright, ytop, ...)
{
  X <- Y <- NULL

  output <- vector(mode = "list", length(xleft))
  for (i in 1:length(xleft))
  {
    roi <- filter_poi(las, X >= xleft[i] & X < xright[i] & Y >= ybottom[i] & Y < ytop[i])
    if (is.empty(roi)) warning(glue::glue("No point found for within rectangle ({xleft[i]}, {ybottom[i]}, {xright[i]}, {ytop[i]})."), call. = FALSE)
    output[[i]] = roi
  }

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
clip_rectangle.LAScatalog = function(las, xleft, ybottom, xright, ytop, ...)
{
  bboxes  <- st_make_bboxes(xleft, xright, ybottom, ytop)
  output  <- catalog_extract(las, bboxes, LIDRRECTANGLE)

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ========  POLYGON ========

#' @export clip_polygon
#' @rdname clip
clip_polygon = function(las, xpoly, ypoly, ...)
{
  assert_is_numeric(xpoly)
  assert_is_numeric(ypoly)
  assert_are_same_length(xpoly, ypoly)

  poly <- sf::st_polygon(list(cbind(xpoly, ypoly)))
  return(clip_roi(las, poly))
}

# ======== CIRCLE ========

#' @export clip_circle
#' @rdname clip
clip_circle = function(las, xcenter, ycenter, radius, ...)
{
  assert_is_numeric(xcenter)
  assert_is_numeric(ycenter)
  assert_is_numeric(radius)
  assert_are_same_length(xcenter, ycenter)
  UseMethod("clip_circle", las)
}

#' @export
clip_circle.LAS = function(las, xcenter, ycenter, radius, ...)
{
  if (length(radius) > 1)
    assert_are_same_length(xcenter, radius)
  else
    radius <- rep(radius, length(xcenter))

  X <- Y <- NULL

  output <- vector(mode = "list", length(xcenter))
  for (i in 1:length(xcenter))
  {
    roi <- filter_poi(las, (X - xcenter[i])^2 + (Y - ycenter[i])^2 <= radius[i]^2)
    if (is.empty(roi)) warning(glue::glue("No point found for within disc ({xcenter[i]}, {ycenter[i]}, {radius[i]})."), call. = FALSE)
    output[[i]] <- roi
  }

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

#' @export
clip_circle.LAScatalog = function(las, xcenter, ycenter, radius, ...)
{
  if (length(radius) > 1)
    assert_are_same_length(xcenter, radius)
  else
    radius <- rep(radius, length(xcenter))

  xmin   <- xcenter - radius
  xmax   <- xcenter + radius
  ymin   <- ycenter - radius
  ymax   <- ycenter + radius

  bboxes <- st_make_bboxes(xmin, xmax, ymin, ymax)
  output <- catalog_extract(las, bboxes, LIDRCIRCLE, ...)

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ======= TRANSECT ========

#' @export
#' @rdname clip
#' @param p1,p2 numeric vectors of length 2 that gives the coordinates of two points that define a
#' transect
#' @param width numeric. width of the transect.
#' @param xz bool. If \code{TRUE} the point cloud is reoriented to fit on XZ coordinates
clip_transect = function(las, p1, p2, width, xz = FALSE, ...)
{
  assert_is_a_bool(xz)

  # Not documented but if p1 and p2 are missing it switches
  # to interactive selection (personal use only)
  if (!missing(p1) && !missing(p2))
  {
    assert_is_numeric(p1)
    assert_is_numeric(p2)
    assert_are_same_length(p1, p2)
    assert_is_of_length(p1, 2L)
  }
  else
  {
    # nocov start
    p <- list(...)
    plot <- isTRUE(p$plot)
    if (plot)
    {
      dsm <- rasterize_canopy(las, 2, p2r())
      plot(las@header)
      plot(dsm, col = height.colors(15))
    }
    tr <- graphics::locator(2L, type = "o")
    p1 <- c(tr$x[1L], tr$y[1L])
    p2 <- c(tr$x[2L], tr$y[2L])
    # nocov end
  }

  if (is(las, "LAScatalog"))
  {
    if (xz && opt_output_files(las) != "")
      stop("Reorientation is not available yet with a LAScatalog", call. = FALSE)
  }

  dx <- p1[1] - p2[1]
  dy <- p1[2] - p2[2]
  a  <- atan(dy/dx)
  rot <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), ncol = 2)
  coords <- rbind(p1, p2)
  line <- sf::st_linestring(coords)
  line <- sf::st_sfc(line)
  sf::st_crs(line) <- sf::st_crs(las)
  poly <- sf::st_buffer(line, dist = width/2, endCapStyle = "FLAT")
  las <- clip_roi(las, poly)

  if (!xz) { return(las) }

  bbox <- st_bbox(las)
  coords <- as.matrix(coordinates(las))
  coords[,1] <- coords[,1] - bbox$xmin
  coords[,2] <- coords[,2] - bbox$ymin
  coords <- coords %*% rot
  X <- coords[,1]
  Y <- coords[,2]
  fast_quantization(X, las@header@PHB[["X scale factor"]], 0)
  fast_quantization(Y, las@header@PHB[["Y scale factor"]], 0)
  las@data[["X"]] <- X
  las@data[["Y"]] <- Y
  las@header@PHB[["X offset"]] <- 0
  las@header@PHB[["Y offset"]] <- 0
  las <- las_update(las)
  data.table::setattr(las, "rotation", rot)
  data.table::setattr(las, "offset", c(bbox$xmin, bbox$ymin))
  return(las)
}

# ======== WKT ========

clip_sf = function(las, sf)
{
  UseMethod("clip_sf", las)
}

clip_sf.LAS = function(las, sf)
{
  sfc <- sf::st_geometry(sf)
  idx <- point_in_polygons(las, sfc, TRUE)
  output <- vector(mode = "list", length(sfc))
  uid <- unique(idx)
  uid <- uid[!is.na(uid)]
  for (i in seq_along(idx))
  {
    # memory optimization
    if(length(idx[[i]])== npoints(las))
    {
      output[[i]] = las
      next
    }

    roi = suppressWarnings(las[idx[[i]]])
    if (is.empty(roi)) warning(glue::glue("No point found for within {sf::st_as_text(sfc[i])}."), call. = FALSE)
    output[[i]] = roi
  }

  if (length(output) == 0)
    return(NULL)
  else if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

clip_sf.LAScatalog = function(las, sf)
{
  wkt  <- sf::st_as_text(sf::st_geometry(sf), digits = 10)

  # We need the bounding box of each geometry to be able to leverage automatically spatial
  # indexing of LAS files with LAX files
  bboxes <- lapply(sf::st_geometry(sf), sf::st_bbox)
  output <- catalog_extract(las, bboxes, LIDRRECTANGLE, sf = sf)

  if (length(output) == 1)
    return(output[[1]])
  else
    return(output)
}

# ============= GENERIC QUERY  =============

#' @param ctg LAScatalog
#' @param bboxes a list of bbox
#' @param shape shape of the query can be a rectangle or a disc
#' @param sf an object of class sf that is used for extracting polygons carrying WKT strings
#' @param data a data.frame carrying some attributes used to create fill the {TEMPLATE}
#' @noRd
catalog_extract = function(ctg, bboxes, shape = LIDRRECTANGLE, sf = NULL, data = NULL)
{
  stopifnot(shape == LIDRRECTANGLE | shape == LIDRCIRCLE)

  if (opt_progress(ctg)) plot.LAScatalog(ctg, mapview = FALSE) # nocov

  # Define a function to be passed in engine_apply
  extract_query = function(cluster)
  {
    if (cluster@files[1] == "")
      return(NULL)

    x <- suppressMessages(suppressWarnings(streamLAS(cluster, ofile = cluster@save, filter_wkt = cluster@wkt)))

    if (is.null(x))
    {
      x <- 0
      class(x) <- "lidr_internal_skip_write"
      return(x)
    }

    index(x) <- index(cluster)
    sensor(x) <- sensor(cluster)
    return(x)
  }

  # Find the ROIs in the catalog and return LASclusters. If a ROI fall outside the catalog
  # its associated LAScluster is NULL and must receive a special treatment in following code
  clusters <- engine_index(ctg, bboxes, shape, 0, outside_catalog_is_null = FALSE)

  # Add some information in the clusters to correctly extract polygons and to write correct file names
  for (i in 1:length(clusters))
  {
    # skip NULL clusters
    if (clusters[[i]]@files[1] == "")
      next

    # If a simple feature is provided we want to extract a polygon. Insert WKT string
    if (!is.null(sf))
      clusters[[i]]@wkt = sf::st_as_text(sf::st_geometry(sf)[i], digits = 10)

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
      X$YCENTER <- format(clusters[[i]]@center$y, scientific = F)
      X$XLEFT   <- format(clusters[[i]]@bbox[1], scientific = F)
      X$XRIGHT  <- format(clusters[[i]]@bbox[3], scientific = F)
      X$YBOTTOM <- format(clusters[[i]]@bbox[2], scientific = F)
      X$YTOP    <- format(clusters[[i]]@bbox[4], scientific = F)
      format    <- if (opt_laz_compression(ctg)) ".laz" else ".las"

      usefilename <- grepl("\\{ORIGINALFILENAME\\}",  opt_output_files(ctg))
      if (usefilename)
        stop("The template {ORIGINALFILENAME} makes sense only when processing by file. It is undefined in clip functions.", call. = FALSE)

      filepath  <- paste0(glue::glue_data(X, opt_output_files(ctg)), format)
      n         <- length(filepath)

      if (n > 1)
        stop(glue::glue("Ill-formed template string in the catalog: {n} filenames were generated for each region of interest"), call. = FALSE)

      clusters[[i]]@save <- filepath
    }
  }

  # Process the cluster using LAScatalog internal engine
  output <- engine_apply(clusters, extract_query, ctg@processing_options, ctg@output_options)

  # output should contain nothing because everything has been streamed into files
  if (opt_output_files(ctg) != "")
  {
    written_path = character(0)
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

    if (length(written_path) > 0)
    {
      new_ctg <- suppressMessages(readLAScatalog(written_path))
      opt_copy(new_ctg) <- ctg
    }
    else
    {
      # Empty LAScatalog
      new_ctg <- new("LAScatalog")
      st_crs(new_ctg) <- st_crs(ctg)
    }

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
        st_crs(output[[i]]) <- st_crs(ctg)
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
