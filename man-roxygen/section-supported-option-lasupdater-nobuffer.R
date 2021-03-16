#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk size}: How much data is loaded at once.
#' \item chunk buffer: A buffer is not need for this function to works thus \code{buffer = 0}
#' and cannot be changed.
#' \item \strong{chunk alignment}: Align the processed chunks.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{output files*}: Mandatory because the output is likely to be too big to be returned
#' in R and needs to be written in las/laz files. Supported templates are \code{\{XLEFT\}}, \code{\{XRIGHT\}},
#' \code{\{YBOTTOM\}}, \code{\{YTOP\}}, \code{\{XCENTER\}}, \code{\{YCENTER\}} \code{\{ID\}} and, if
#' chunk size is equal to 0 (processing by file), \code{\{ORIGINALFILENAME\}}.
#' \item select: The function will write files equivalent to the original ones. Thus \code{select = "*"}
#' and cannot be changed.
#' \item \strong{filter}: Read only points of interest.
#' }
