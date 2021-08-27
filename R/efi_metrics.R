#' Computes metrics for each plot of a ground inventory
#'
#' Computes metrics for each plot of a ground inventory by 1. clipping the plots inventories 2. computing
#' user's metrics to each plot 3. combining spatial data and metrics into one data.frame ready for
#' statistical modelling. `efi_metrics` is basically a seamless wrapper around \link{clip_roi},
#' \link{cloud_metrics}, `cbind` and adequate processing settings.
#'
#' @template param-las
#' @param func formula. An expression to be applied to each cell (see also \link{grid_metrics}).
#' @param geometry a spatial object. Many types are supported (see also \link{clip_roi})
#' @param ... optional supplementary options (see also \link{clip_roi})
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' SHPfile <- system.file("extdata", "efi_plot.shp", package="lidR")
#' las <- readLAS(LASfile)
#' inventory <- sf::st_read(SHPfile, quiet = TRUE)
#' inventory # contains an ID and a Value Of Interest (VOI) per plot
#'
#' M <- efi_metrics(las, ~list(q85 = quantile(Z, probs = 0.85)), inventory, radius = 11.28)
#' model <- lm(VOI ~ q85, M)
#'
#' M <- efi_metrics(las, .stdmetrics_z, inventory, radius = 11.28)
#'
#' \dontrun{
#' # Works with polygons as well
#' inventory <- sf::st_buffer(inventory, 11.28)
#' plot(las@header)
#' plot(sf::st_geometry(inventory), add = TRUE)
#' M <- efi_metrics(las, .stdmetrics_z, inventory)
#' }
#' @return A `data.frame` with all the metrics for each plot as well as all the attributes of the spatial
#' object used to query the inventory such as plot ID, basal area, above ground biomass
#' @export
#' @family metrics
efi_metrics <- function(las, func, geometry, ...)
{
  rois <- clip_roi(las, geometry, ...)

  if (is(rois, "LAScatalog"))
  {
    opt_independent_files(rois) <- TRUE
    opt_output_files(rois) <- ""
    metrics <- catalog_apply(rois, cloud_metrics, func = func)
  }
  else
  {
    metrics <- lapply(rois, cloud_metrics, func = func)
  }

  metrics <- data.table::rbindlist(metrics)
  data.table::setDF(metrics)

  if (is(geometry, "sf"))
    ToA <- sf::st_drop_geometry(geometry)
  else
    ToA <- geometry@data

  metrics <- cbind(ToA, metrics)
  return(metrics)
}
