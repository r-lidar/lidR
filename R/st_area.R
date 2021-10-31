#' Surface covered by a LAS* object
#'
#' Surface covered by a `LAS*` object. For `LAS` point clouds it is computed based on the
#' convex hull of the points. For a `LAScatalog` it is computed as the sum of the bounding boxes
#' of the files. For overlapping tiles the value may be larger than the total covered area because
#' some regions are sampled twice. For a `LASheader` it is computed with the bounding box.
#' As a consequence for the same file `st_area` applied on a LASheader or on a LAS can return
#' slightly different values. `st_area()` extends `sf:st_area()`, `area()` extends
#' `raster:area()`. `area()` is provided for backward compatibility..
#'
#' @param x An object class \code{LAS*}.
#' @param ... unused.
#'
#' @return numeric. A number.in the same units of the coordinate reference system.
#'
#' @export
#' @name st_area
#' @importFrom sf st_area
#' @importFrom raster area
#' @md
NULL

#' @export
#' @rdname st_area
st_area.LAS = function(x, ...)
{
  if (npoints(x) == 0L) { return(0) }
  return(sf::st_area(st_convex_hull(x)))
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
