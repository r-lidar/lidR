#' @rdname engine
#' @export
#' @param x LAS, Raster, stars, SpatRaster,sf, sfc, Spatial
#' @param bbox bbox
engine_crop <- function(x, bbox)
{
  UseMethod("engine_crop", x)
}

#' @export
engine_crop.LAS <- function(x, bbox)
{
  buffer <- NULL
  x <- filter_poi(x, buffer == LIDRNOBUFFER)
  if (is.empty(x)) return(NULL)
  return(x)
}

#' @export
engine_crop.Raster <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

#' @export
engine_crop.stars <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

#' @export
engine_crop.SpatRaster <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

#' @export
engine_crop.sf <- function(x, bbox)
{
  return(sf::st_crop(x, bbox))
}

#' @export
engine_crop.sfc <- function(x, bbox)
{
  return(sf::st_crop(x, bbox))
}

#' @export
engine_crop.Spatial <- function(x, bbox)
{
  x <- sf::st_as_sf(x)
  x <- sf::st_crop(x, bbox)
  return(sf::as_Spatial(x))
}

#' @export
engine_crop.default <- function(x, bbox)
{
  stop(glue::glue("No method for cropping '{class(x)}'. Use catalog_apply instead of catalog_map."), call. = FALSE)
}
