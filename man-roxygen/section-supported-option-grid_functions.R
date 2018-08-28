#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: How many data are loaded at once. \code{tiling_size = 0} is not allowed
#' to guarantee a stric wall-to-wall continous output.
#' \item buffer: This function guarantee a stric wall-to-wall continous output. The \code{buffer} option
#' is no considered.
#' \item \strong{alignment}: Align the processed clusters
#' \item \strong{cores}: How many cores are used. .
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{stop_early}: Leave it as it unless you are an advanced user.
#' \item \strong{output_files}: Return the output in R or write each cluster's output in a file.
#' \item laz_compression: is not supported because this function will never write \code{las/laz} files
#' \item \strong{drivers}: Leave it as it unless you are an advanced user.
#' \item select: The function knows for you what should be loaded or not.
#' \item \strong{filter}: Read only points of interest.
#' }
