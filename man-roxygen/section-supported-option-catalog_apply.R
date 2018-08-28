#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: How many data are loaded at once.
#' \item \strong{buffer}: Load clusters with a buffer
#' \item \strong{alignment}: Align the processed clusters
#' \item \strong{cores}: How many cores are used. .
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{stop_early}: Leave it as it unless you are an advanced user.
#' \item \strong{output_files}: The user-function outputs will be written in files instead of being
#' returned into R
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files (only if the user-defined function)
#' return a \code{LAS} object.
#' \item \strong{drivers}: Leave it as it unless you are an advanced user.
#' \item \strong{select}: Select only the data of interest to save processing memory.
#' \item \strong{filter}: Read only points of interest.
#' }
