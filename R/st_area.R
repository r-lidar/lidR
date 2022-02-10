#' Surface covered by a LAS* object
#'
#' Surface covered by a `LAS*` object. For `LAS` point clouds it is computed based on the number of
#' occupied cells, or on the area of the convex hull of the points when the density is too low. For a
#' `LAScatalog` it is computed as the sum of the bounding boxes of the files. For overlapping tiles
#' the value may be larger than the total area covered because some regions are sampled twice.
#' For a `LASheader` it is computed with the bounding box. As a consequence, for the same file
#' `st_area` applied on a LASheader or on a LAS can return slightly different values. `st_area()`
#' extends `sf:st_area()`, `area()` extends `raster:area()`. `area()` is provided for backward
#' compatibility.
#'
#' @param x An object of class \code{LAS*}.
#' @param ... unused.
#'
#' @return numeric. A number in the same units as the coordinate reference system.
#'
#' @export
#' @name st_area
#' @importFrom sf st_area
#' @importFrom terra area
#' @md
NULL

#' @export
#' @rdname st_area
st_area.LAS = function(x, ...)
{
  if (npoints(x) == 0L) { return(0) }

  d <- density(header(x))

  if (d < 0.5)
    return(sf::st_area(st_convex_hull(x)))

  if (st_crs(x) == sf::NA_crs_)
    r <- 4
  else if (sf::st_is_longlat(x))
    return(sf::st_area(st_convex_hull(x)))
  else if (st_proj_is_meters(x))
    r <- 2
  else
    r <- 6

  temp  <- raster_layout(x, r)
  cells <- raster_cell_from_xy(temp, x$X, x$Y)
  n <- data.table::uniqueN(cells)
  area <- sum(n)*r^2

  # Workaround to get area with units, but without depending directly on units
  # and without parsing the CRS.
  bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = sqrt(area), ymax = sqrt(area)), crs = st_crs(x)))

  return(sf::st_area(bbox))
}

#' @export
#' @rdname st_area
st_area.LASheader = function(x, ...)
{
  bbox <- st_bbox(x)
  bbox <- sf::st_as_sfc(bbox)
  return(sf::st_area(bbox))
}

#' @export
#' @rdname st_area
st_area.LAScatalog = function(x, ...)
{
  x <- lascatalog_v3_repair(x)

  if (nrow(x@data) == 0L) { return(0) }
  areas <- sf::st_area(x@data)
  return(sum(areas))
}

#' @export
#' @rdname st_area
setMethod("area", "LAS", function(x, ...) { as.numeric(st_area(x)) })

#' @export
#' @rdname st_area
setMethod("area", "LASheader", function(x, ...) { as.numeric(st_area(x)) })

#' @export
#' @rdname st_area
setMethod("area", "LAScatalog",  function(x, ...) { as.numeric(st_area(x)) })
