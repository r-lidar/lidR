#' @param geometry A spatial vector object. `sp` and `sf`' objects are supported. `plot_metrics()`
#' supports point and polygons but `polygon_metrics()` supports only polygons.
#' @param radius numeric. If the geometry is spatial points a radius must be defined. Support one
#' radius or a vector of radii for variable plot sizes.
#' @rdname aggregate
#' @export
#' @md
plot_metrics <- function(las, func, geometry, ..., radius)
{
  if (missing(radius))
    rois <- suppressWarnings(clip_roi(las, geometry))
  else
    rois <- suppressWarnings(clip_roi(las, geometry, radius = radius))

  if (is(rois, "LAScatalog"))
  {
    opt_independent_files(rois) <- TRUE
    opt_output_files(rois) <- ""
    opt_progress(rois) <- FALSE
    metrics <- catalog_apply(rois, cloud_metrics, func = func, ...)
  }
  else
  {
    # Fix 664: clip_roi returns inconsistently a LAS or a list depending if there is
    # one or muliple queries. If there is only one query rois in not a list and sapply fails
    if (is(rois, "LAS"))
      rois = list(rois)

    n <- length(rois)
    empty <- sapply(rois, is.empty)
    any_empty <- any(empty)
    if (any_empty)
    {
      k <- which(empty)
      warning(glue::glue("Plots [{glue::glue_collapse(k, sep = ', ')}] are empty."), call. = FALSE)
      rois <- rois[-k]
    }

    metrics <- lapply(rois, template_metrics, template = NULL, func = func, ...)

    if (any_empty)
    {
      incomplete_metrics <- metrics
      ref <- metrics[[1]]
      metrics <- vector("list", n)
      ref <- lapply(ref, function(x) return(NA))
      j = 1
      for (i in 1:n)
      {
        if (empty[i])
        {
          metrics[[i]] <- ref
        }
        else
        {
          metrics[[i]] <- incomplete_metrics[[j]]
          j = j+1
        }
      }

    }
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
