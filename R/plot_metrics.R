#' Computes metrics for each plot of a ground inventory
#'
#' Computes metrics for each plot of a ground inventory by 1. clipping the plots inventories, 2. computing
#' a user's metrics to each plot, and 3. combining spatial data and metrics into one data.frame ready for
#' statistical modelling. `plot_metrics` is basically a seamless wrapper around \link{clip_roi},
#' \link{cloud_metrics}, `cbind` and adequate processing settings.
#'
#' @template param-las
#' @param func formula. An expression to be applied to each cell (see also \link{grid_metrics}).
#' @param geometry a spatial object. Many types are supported (see also \link{clip_roi})
#' @param ... optional supplementary options (see also \link{clip_roi})
#'
#' @template LAScatalog
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} in \code{plot_metrics} function (in bold). For
#' more details see the \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item chunk size: Not relevant here.
#' \item chunk buffer: Not relevant here.
#' \item chunk alignment: Not relevant here.
#' \item \strong{progress}: Displays a progress estimate.
#' \item output files: plots are extracted in memory.
#' \item \strong{select}: Read only the attributes of interest.
#' \item \strong{filter}: Read only the points of interest.
#' }

#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' SHPfile <- system.file("extdata", "efi_plot.shp", package="lidR")
#' las <- readLAS(LASfile)
#' inventory <- sf::st_read(SHPfile, quiet = TRUE)
#' inventory # contains an ID and a Value Of Interest (VOI) per plot
#'
#' M <- plot_metrics(las, ~list(q85 = quantile(Z, probs = 0.85)), inventory, radius = 11.28)
#' model <- lm(VOI ~ q85, M)
#'
#' M <- plot_metrics(las, .stdmetrics_z, inventory, radius = 11.28)
#'
#' \dontrun{
#' # Works with polygons as well
#' inventory <- sf::st_buffer(inventory, 11.28)
#' plot(las@header)
#' plot(sf::st_geometry(inventory), add = TRUE)
#' M <- plot_metrics(las, .stdmetrics_z, inventory)
#' }
#' @return An `sp` or `sf` object depending on the input with all the metrics for each plot bound
#' with the original input.
#' @export
#' @family metrics
plot_metrics <- function(las, func, geometry, ...)
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
  metrics <- cbind(geometry, metrics)
  return(metrics)
}
