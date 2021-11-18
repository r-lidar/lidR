#' @param area numeric. Area of the hexagons
#' @rdname aggregate
#' @export
hexagon_metrics <- function(las, func, area = 400, ...) { UseMethod("hexagon_metrics", las) }

#' @export
hexagon_metrics.LAS <- function(las, func, area = 400, ...)
{
  assert_is_a_number(area)
  assert_all_are_non_negative(area)

  # hexagon area : A = sqrt(3)/2*width^2
  # width = sqrt(2*A/sqrt(3))
  width <- sqrt(2*area/sqrt(3))
  sfc = sf::st_as_sfc(st_bbox(las))
  template = sf::st_make_grid(sfc, cellsize = width, square = FALSE)
  return(template_metrics(las, func, template, ...))
}

#' @export
hexagon_metrics.LAScluster <- function(las, func, area = 400, ...)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  metrics <- hexagon_metrics(x, func, area, ...)

  bbox <- st_bbox(las)
  sf::st_agr(metrics) <- "constant"
  centroid <- sf::st_centroid(metrics)
  centroid$ID <- 1:nrow(centroid)
  sf::st_agr(centroid) <- "constant"
  centroid <- sf::st_crop(centroid, bbox)
  return(metrics[centroid$ID,])
}

#' @export
hexagon_metrics.LAScluster <- function(las, func, area = 400, ...)
{
  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  globals <- future::getGlobalsAndPackages(func)

  options <- list(need_buffer = FALSE, drop_null = TRUE, globals = names(globals$globals), automerge = TRUE)
  output  <- catalog_apply(las, hexagon_metrics, func = func, area = area, ..., .options = options)
  return(output)
}
