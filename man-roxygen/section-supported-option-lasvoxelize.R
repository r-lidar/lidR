#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk size}: How much data is loaded at once.
#' \item chunk buffer: This function guarantees a strict continuous wall-to-wall output. The \code{buffer} option
#' is not considered.
#' \item \strong{chunk alignment}: Align the processed chunks.
#' \item \strong{cores}: How many cores are used. More cores means more data is loaded at once.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{output_files*}: Mandatory because the output is likely to be too big to be returned
#'  in R and needs to be written in las/laz files. Supported templates are \code{\{XLEFT\}}, \code{\{XRIGHT\}},
#' \code{\{YBOTTOM\}}, \code{\{YTOP\}}, \code{\{XCENTER\}}, \code{\{YCENTER\}} \code{\{ID\}} and, if
#' chunk size is equal to 0 (processing by file), \code{\{ORIGINALFILENAME\}}.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files
#' \item select: The function loads \code{"xyzi"} only.
#' \item \strong{filter}: Read only points of interest.
#' }
