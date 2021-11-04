#' Bounding box of a LAS* object
#'
#' Bounding box of a LAS* object. `st_bbox()` extends `sf:st_bbox()`, `extent()` extends
#' `raster:extent()`, `bbox()` extends `sp:bbox()`. The value returned are similar to their parent
#' functions. `bbox()` and `extent()` are provided for backward compatibility..
#'
#' @return A `bbox` from sf, an `Extent` from raster or a `matrix` from sp.
#'
#' @param obj,x An object of class \code{LAS*}.
#' @param ... unused
#'
#' @export
#' @name st_bbox
#' @importFrom sf st_bbox
#' @importFrom sp bbox
#' @importFrom raster extent
#' @md
NULL

#' @export
#' @rdname st_bbox
st_bbox.LAS = function(obj, ...)
{
  xr <- range(obj$X)
  yr <- range(obj$Y)
  bbox <- st_bbox(c(xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2]))
  sf::st_crs(bbox) <- st_crs(obj)
  return(bbox)
}

#' @export
#' @rdname st_bbox
st_bbox.LASheader = function(obj, ...)
{
  if (obj[["Number of point records"]] == 0L)
    return(sf::NA_bbox_)

  bbox <- c(obj[["Min X"]], obj[["Min Y"]], obj[["Max X"]], obj[["Max Y"]])
  names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
  crs <- tryCatch(crs <- st_crs(obj), error = function(e) sf::NA_crs_)
  return(sf::st_bbox(bbox, crs = crs))
}

#' @export
#' @rdname st_bbox
st_bbox.LAScatalog = function(obj, ...)
{
  # Workaround to repair LAScatalog v3 and minimize backward incompatibilities with v4
  obj <- lascatalog_v3_repair(obj)
  return(sf::st_bbox(obj@data))
}

#' @export
#' @rdname st_bbox
st_bbox.LAScluster = function(obj, ...)
{
  bb <- as.numeric(obj@bbox)
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bb <- sf::st_bbox(bb)
  sf::st_crs(bb) <- st_crs(obj)
  return(bb)
}

st_bbox.raster_template = function(obj, ...)
{
  bb <- c(obj$xmin, obj$ymin, obj$xmax, obj$ymax)
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bb <- sf::st_bbox(bb)
  sf::st_crs(bb) <- obj$crs
  return(bb)
}

# ==== EXTENT =====

#' @export
#' @rdname st_bbox
setMethod("extent", "LAS", function(x, ...) { .extent(x) })

#' @export
#' @rdname st_bbox
setMethod("extent", "LASheader", function(x, ...) { .extent(x) })

#' @export
#' @rdname st_bbox
setMethod("extent", "LAScatalog", function(x, ...) { .extent(x) })

#' @export
#' @rdname st_bbox
setMethod("extent", "LAScluster", function(x, ...) { .extent(x) })

.extent <- function(x)
{
  bb <- st_bbox(x)
  return(raster::extent(bb$xmin, bb$xmax, bb$ymin, bb$ymax))
}

# ==== BBOX =====

#' @export
#' @rdname st_bbox
setMethod("bbox", "LAS", function(obj) { .bbox(obj) })

#' @export
#' @rdname st_bbox
setMethod("bbox", "LASheader", function(obj) { .bbox(obj) })

#' @export
#' @rdname st_bbox
setMethod("bbox", "LAScatalog", function(obj) { .bbox(obj) })

#' @export
#' @rdname st_bbox
setMethod("bbox", "LAScluster", function(obj) { .bbox(obj) })

.bbox <- function(obj)
{
  bb <- st_bbox(obj)
  return(matrix(c(bb$xmin, bb$xmax, bb$ymin, bb$ymax), ncol = 2, byrow = TRUE))
}
