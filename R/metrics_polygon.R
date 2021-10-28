#' @rdname template_metrics
#' @export
polygon_metrics = function(las, func, geometry, ...)
{
  UseMethod("polygon_metrics", las)
}

#' @export
polygon_metrics.LAS = function(las, func, geometry, ...)
{
  stopifnot(is(geometry, "sf") | is(geometry, "sfc"))
  geometry <- sf::st_geometry(geometry)
  M <- template_metrics(las, func, geometry, ...)
  return(geometry)
}
