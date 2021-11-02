#' @param geometry a spatial vector object. `sp` and `sf`' objects are supported
#' @param radius numeric. If the geometry is spatial points as radius must be defined
#' @rdname aggregate
#' @export
plot_metrics <- function(las, func, geometry, ..., radius)
{
  if (missing(radius))
    rois <- clip_roi(las, geometry)
  else
    rois <- clip_roi(las, geometry, radius = radius)

  if (is(rois, "LAScatalog"))
  {
    opt_independent_files(rois) <- TRUE
    opt_output_files(rois) <- ""
    metrics <- catalog_apply(rois, cloud_metrics, func = func, ...)
  }
  else
  {
    metrics <- lapply(rois, template_metrics, template = NULL, func = func, ...)
  }

  if (length(metrics[[1]]) == 1)
  {
    name <- names(metrics[[1]])
    if (is.null(name)) name = "V1"
    metrics <- data.table::data.table(unlist(metrics))
    names(metrics) <- name
  }
  else
  {
    metrics <- data.table::rbindlist(metrics)
  }

  data.table::setDF(metrics)
  metrics <- cbind(geometry, metrics)
  return(metrics)
}
