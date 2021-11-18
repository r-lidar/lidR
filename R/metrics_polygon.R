#' @rdname aggregate
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
  stopifnot(is(geometry, "sfc_POLYGON") | is(geometry, "sfc_MULTIPOLYGON"))
  M <- template_metrics(las, func, geometry, ...)
  M <- sf::st_set_geometry(M, geometry)
  return(M)
}
