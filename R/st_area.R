#' Surface covered by a LAS* object
#'
#' Surface covered by a `LAS*` object. The surface covered by a point cloud is mathematically 0.
#' To compute non zero values the function uses different strategies. The area is computed based on
#' the number of occupied cells, or on the area of the convex hull of the points depending on the density
#' and the size of the point cloud. The result is necessarily an approximation that depends on the method
#' used.\cr For a `LAScatalog` it is computed as the sum of the bounding boxes of the files. For
#' overlapping tiles the value may be larger than the total area covered because some regions are
#' sampled twice. For a `LASheader` it is computed with the bounding box. As a consequence, for the same file
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
#' @md
NULL

#' @export
#' @rdname st_area
st_area.LAS = function(x, ...)
{
  if (npoints(x) == 0L) { return(0) }

  r <- area_strategy(x)

  if (r == 0)
    return(round(sf::st_area(st_convex_hull(x)), 1))

  lay  <- raster_layout(x, r)
  temp <- fasterize(x, lay, method = "count")
  area <- sum(!is.na(temp))*r^2

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

#' @importFrom terra area
#' @export
#' @rdname st_area
setMethod("area", "LAS", function(x, ...) { as.numeric(st_area(x)) })

#' @export
#' @rdname st_area
setMethod("area", "LASheader", function(x, ...) { as.numeric(st_area(x)) })

#' @export
#' @rdname st_area
setMethod("area", "LAScatalog",  function(x, ...) { as.numeric(st_area(x)) })


# The area covered by a point cloud is 0 m2. To get another value we must apply a measurement
# strategy. Different strategies return different values. The following function tries to
# infer the more relevant strategy.
area_strategy <- function(x)
{
  bbarea  <- area(header(x))
  npoints <- npoints(x)
  density <- npoints/bbarea
  unitsm  <- st_proj_is_meters(x)
  r <- 0

  # For longlat CRS sf::st_area on the convex hull will return something
  # in the good units. We do not use occupancy grid. Returning 0.
  # For low density, Occupancy grid does not makes sense
  # compute with convex hull (return a resolution of 0)
  # For very tiny point cloud (typically plots) we do not use occupancy grid
  if (isTRUE(sf::st_is_longlat(x)) | density < 0.5 | bbarea < 1000)  return(r)

  # In other cases we use the occupancy grid to estimate the area. The following estimate the best resolution.
  # LAstools uses 2 m or 6 ft (1.8 m) depending of the units. For unknow units we are using 4 (between 2 and 6).
  if (st_crs(x) == sf::NA_crs_)
    r <- 4
  else if (unitsm)
    r <- 2
  else
    r <- 6

  # Previous resolution are good for large dataset. The overestimation is minor. For small coverage
  # the overestimation is significant. See also
  # https://stackoverflow.com/questions/71362024/how-to-get-a-circular-subset-of-a-las-dataset-with-specific-area-using-lidrcli/71369824#71369824
  # If the header tells us the area is less than 1 ha, use a finer resolution for more accuracy. But
  # only if the density if sufficient to compute occupancy grid at this resolution (i.e. there is
  # statically more than 1 point per cell). The threshold is 1.5 point per sq meters.
  one_ha <- 10000
  if (st_crs(x) == sf::NA_crs_ && bbarea < one_ha && density > 3/8)
    r <- r/2
  else if (unitsm && bbarea < one_ha && density > 3/2)
    r <- r/2
  else if (!unitsm && bbarea < one_ha*10.7 && density > 3/18)
    r <- r/2

  return(r)
}
