#' @rdname aggregate
#' @name aggregate
#' @export
cloud_metrics = function(las, func, ...)
{
  UseMethod("cloud_metrics", las)
}

#' @export
cloud_metrics.LAS = function(las, func, ...)
{
  metrics <- template_metrics(las, func, st_bbox(las), ...)
  if (length(metrics) == 1L) return(metrics[[1]])
  return(metrics)
}

#' @export
cloud_metrics.LAScluster = function(las, func, ...)
{
  las <- readLAS(las)
  if (is.empty(las)) return(NULL)
  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  return(cloud_metrics(las, func,...))
}

