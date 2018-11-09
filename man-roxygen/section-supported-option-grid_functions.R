#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} in \code{grid_*} functions (in bold). For
#' more details see the \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: How much data is loaded at once. The tiling size may be slightly modified
#' internally to ensure a strict continuous wall-to-wall output even when tiling size is equal to 0 (processing
#' by file).
#' \item buffer: This function guarantees a strict continuous wall-to-wall output. The \code{buffer} option
#' is not considered.
#' \item \strong{alignment}: Align the processed clusters. The alignment may be slightly modified
#' internally to ensure a strict continuous wall-to-wall output.
#' \item \strong{cores}: How many cores are used.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{output_files}: Return the output in R or write each cluster's output in a file. Supported
#' templates are \code{XLEFT}, \code{XRIGHT}, \code{YBOTTOM}, \code{YTOP}, \code{XCENTER}, \code{YCENTER}
#' \code{ID} and, if tiling size equal to 0 (processing by file), \code{ORIGINALFILENAME}.
#' \item laz_compression: is not supported because this function will never write \code{las/laz} files
#' \item \strong{\emph{select}}: The functions \code{grid_*} usually 'know' what should be loaded
#' and this option is not considered. In \link{grid_metrics} this option is respected.
#' \item \strong{filter}: Read only points of interest.
#' }
