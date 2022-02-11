#' Merge a point cloud with a source of spatial data
#'
#' Merge a point cloud with a source of spatial data. It adds an attribute along each point based on
#' a value found in the spatial data. Sources of spatial data can be a `SpatialPolygons*`, an `sf`/`sfc`,
#' a `Raster*`, a `stars`, or a `SpatRaster`.\cr
#' \itemize{
#' \item{`SpatialPolygons*`, `sf` and `sfc`: it checks if the points belongs within each polygon. If
#' the parameter `attribute` is the name of an attribute in the table of attributes it assigns
#' to the points the values of that attribute. Otherwise it classifies the points as boolean.
#' TRUE if the points are in a polygon, FALSE otherwise.}
#' \item{`RasterLayer`, single band `stars` or single layer `SpatRaster`: it attributes to each point
#' the value found in each pixel of the raster}.
#' \item{`RasterStack`, `RasterBrick`, multibands `stars` or multilayer `SpatRaster` must have 3
#' layers for RGB colors. It colorizes the point cloud with RGB values.}
#' }
#'
#' @param las An object of class `LAS`
#' @param source An object of class `SpatialPolygons*` or `sf` or `sfc` or `RasterLayer` or
#' `RasterStack` or `RasterBrick` or `stars`.
#' @param attribute character. The name of an attribute in the table of attributes or
#' the name of a new column in the LAS object. Not relevant for RGB colorization.
#'
#' @return a `LAS` object
#'
#' @export
#' @md
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shp     <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
#'
#' las   <- readLAS(LASfile, filter = "-keep_random_fraction 0.1")
#' lakes <- sf::st_read(shp, quiet = TRUE)
#'
#' # The attribute "inlake" does not exist in the shapefile.
#' # Points are classified as TRUE if in a polygon
#' las    <- merge_spatial(las, lakes, "inlakes")     # New attribute 'inlakes' is added.
#' names(las)
#'
#' forest <- filter_poi(las, inlakes == FALSE)
#' #plot(forest)
#'
#' # The attribute "LAKENAME_1" exists in the shapefile.
#' # Points are classified with the values of the polygons
#' las <- merge_spatial(las, lakes, "LAKENAME_1")     # New column 'LAKENAME_1' is added.
#' names(las)
merge_spatial = function(las, source, attribute = NULL)
{
  UseMethod("merge_spatial", las)
}

#' @export
merge_spatial.LAS = function(las, source, attribute = NULL)
{
  if (is_raster(source) && !raster_nlayer(source) %in% c(1,3))
    stop("rasters must have 1 or 3 bands", call. = FALSE)

  if (inherits(source, "Spatial"))
    source <- sf::st_as_sf(source)

  rgb <- is_raster(source) && raster_nlayer(source) == 3L

  if (is(source, "sf") | is(source, "sfc"))
  {
    geom <- sf::st_geometry(source)
    if (!is(geom, "sfc_POLYGON") & !is(geom, "sfc_MULTIPOLYGON"))
      stop("Only polygon geometry types are supported", call. = FALSE)

    # crop to bbox of las to reduce computation time
    source <- st_crop_if_not_similar_bbox(source, las)
  }

  if (is(source, "sf"))
    values <- merge_sf(las, source, attribute)
  else if (is(source, "sfc"))
    values <- merge_sfc(las, source, attribute)
  else if (is_raster(source) && !rgb)
    values <- raster_value_from_xy(source, las$X, las$Y)
  else if (is_raster(source) && rgb)
    return(colorize_points(las, source))
  else
    stop("No method for this source format.")

  if (is.null(attribute))
    attribute <- "id"

  las <- add_attribute(las, values, attribute)
  return(las)
}

colorize_points = function(las, source)
{
  cells <- raster_cell_from_xy(source, las$X, las$Y)

  R <- raster_value_from_cells(source, cells, 1L)
  G <- raster_value_from_cells(source, cells, 2L)
  B <- raster_value_from_cells(source, cells, 3L)
  R <- as.integer(R)
  G <- as.integer(G)
  B <- as.integer(B)

  if (anyNA(R) | anyNA(G) | anyNA(B))
    stop("Some points were associated with an NA RGB color. RGB cannot be NA in a LAS object. Colorization aborted.", call. = FALSE)

  return(add_lasrgb(las, R, G, B))
}

merge_sf = function(las, source, attribute = NULL)
{
  npoints <- npoints(las)

  # No attribute is provided: assign the number of the polygon
  if (is.null(attribute))
  {
    method <- 0
    values <- rep(NA_integer_, npoints)
  }
  # The attribute is the name of an attribute in the attribute table: assign the value of the attribute
  else if (attribute %in% names(source))
  {
    method <- 1
    data   <- source[[attribute]]

    if (class(data) == "factor")
      values = factor(rep(NA_integer_, npoints), levels = levels(data))
    else if (class(data) == "integer")
      values = rep(NA_integer_, npoints)
    else if (class(data) == "logical")
      values = rep(NA, npoints)
    else if (class(data) == "numeric")
      values = rep(NA_real_, npoints)
    else if (class(data) == "character")
      values = rep(NA_character_, npoints)
    else
      stop(glue::glue("The attribute {attribute} in the table of attributes is not of a supported type."))
  }
  # The attribute is not the name of an attribute in the attribute table: assign a boolean value if
  # the point is in a polygon or not.
  else
  {
    method <- 2
    values <- logical(npoints)
  }

  if (nrow(source) == 0)
    return(values)

  verbose("Testing whether points fall in a given polygon...")

  idx <- point_in_polygons(las, sf::st_geometry(source))

  if (method == 1)
    values[!is.na(idx)] <- source[[attribute]][idx[!is.na(idx)]]
  else if (method == 2)
    values <- !is.na(idx)
  else
    values <- idx

  return(values)
}

merge_sfc = function(las, source, attribute = NULL)
{
  npoints <- npoints(las)

  # No attribute is provided: assign the number of the polygon
  if (is.null(attribute)) {
    method <- 0
    values <- rep(NA_integer_, npoints)
  } else {
    method <- 2
    values <- logical(npoints)
  }

  if (length(source) == 0)
    return(values)


  sfc <- sf::st_geometry(source)
  ids <- point_in_polygons(las, sfc)

  if (method == 2)
    values <- !is.na(ids)
  else
    values <- ids

  return(values)
}

