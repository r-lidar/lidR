#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk size}: How much data is loaded at once.
#' \item \strong{chunk buffer*}: Mandatory to get a continuous output without edge effects. The buffer is
#' always removed once processed and will never be returned either in R or in files.
#' \item \strong{chunk alignment}: Align the processed chunks.
#' \item \strong{progress}: Displays a progression estimation.
#' \item output_files: Saving intermediate results is disabled in 'sensor_tracking' because the output
#' must be post-processed as a whole.
#' \item laz_compression: write \code{las} or \code{laz} files
#' \item select: is not supported. It is set by default to "xyzrntp"
#' \item \strong{filter}: Read only points of interest. By default it uses "-drop_single" but user can
#' add filters such as "-thin_pulses_with_time 0.001" to reduce the number of points loaded. There is
#' no gain at computing the sensor position with too many pulses.
#' }
