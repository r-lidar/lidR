#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} in \code{grid_*} functions (in bold). For
#' more details see the \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk size}: How much data is loaded at once. The chunk size may be slightly modified
#' internally to ensure a strict continuous wall-to-wall output even when chunk size is equal to 0
#' (processing by file).
#' \item chunk buffer: This function guarantees a strict continuous wall-to-wall output. The
#' \code{buffer} option is not considered.
#' \item \strong{chunk alignment}: Align the processed chunks. The alignment may be slightly modified
#' internally to ensure a strict continuous wall-to-wall output.
#' \item \strong{progress}: Displays a progress estimate.
#' \item \strong{output files}: Return the output in R or write each cluster's output in a file.
#' Supported templates are \code{\{XLEFT\}}, \code{\{XRIGHT\}}, \code{\{YBOTTOM\}}, \code{\{YTOP\}},
#' \code{\{XCENTER\}}, \code{\{YCENTER\}} \code{\{ID\}} and, if chunk size is equal to 0 (processing
#' by file), \code{\{ORIGINALFILENAME\}}.
#' \item \strong{\emph{select}}: The \code{grid_*} functions usually 'know' what should be loaded
#' and this option is not considered. In \link{grid_metrics} this option is respected.
#' \item \strong{filter}: Read only the points of interest. \link{grid_terrain} used \code{-keep_class}
#' by default with the class mentionned in \code{use_class}.
#' }
