#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} in \code{grid_*} functions (in bold). For
#' more details see the \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: How much data are loaded at once. The tiling size may be slightly modified
#' internally to ensure a strict wall-to-wall continuous output even when tiling size equal to 0 (processing
#' by file).
#' \item buffer: This function guarantee a stric wall-to-wall continuous output. The \code{buffer} option
#' is no considered.
#' \item \strong{alignment}: Align the processed clusters. The alignment may be slightly modified
#' internally to ensure a strict wall-to-wall continuous output.
#' \item \strong{cores}: How many cores are used.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{stop_early}: Leave it as it unless you are an advanced user.
#' \item \strong{output_files}: Return the output in R or write each cluster's output in a file.
#' \item laz_compression: is not supported because this function will never write \code{las/laz} files
#' \item \strong{drivers}: Leave it as it unless you are an advanced user.
#' \item \strong{\emph{select}}: The functions \code{grid_*} usually know for you what should be loaded or not
#' and this options is not considered. In \link{grid_metric} this option is respected.
#' \item \strong{filter}: Read only points of interest.
#' }
