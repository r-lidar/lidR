engine_crop <- function(x, bbox)
{
  UseMethod("engine_crop", x)
}

engine_crop.LAS <- function(x, bbox)
{
  buffer <- NULL
  return(filter_poi(x, buffer == LIDRNOBUFFER))
}

engine_crop.Raster <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

engine_crop.stars <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

engine_crop.SpatRaster <- function(x, bbox)
{
  return(raster_crop(x, bbox))
}

engine_crop.sf <- function(x, bbox)
{
  return(sf::st_crop(x, bbox))
}

engine_crop.sfc <- function(x, bbox)
{
  return(sf::st_crop(x, bbox))
}

engine_crop.Spatial <- function(x, bbox)
{
  x <- sf::st_as_sf(x)
  x <- sf::st_crop(x, bbox)
  return(sf::as_Spatial(x))
}

engine_crop.default <- function(x, bbox)
{
  stop(glue::glue("No method for cropping '{class(x)}'. Use catalog_apply instead of catalog_map."), call. = FALSE)
}
