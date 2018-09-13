#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{tiling_size}: How many data are loaded at once.
#' \item \strong{buffer*}: Mandatory to get a continuous output without edge effect. The buffer is
#' always removed once processed and will never be returned neither in R nor in files.
#' \item \strong{alignment}: Align the processed clusters
#' \item \strong{cores}: How many cores are used. More cores means more data loaded at once.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{stop_early}: Leave it as it unless you are an advanced user.
#' \item \strong{output_files*}: Mandatory because the output is likely to be too big to be returned
#'  in R and needs to be written in las/laz files. Supported templates are \code{\{XLEFT\}}, \code{\{XRIGHT\}},
#' \code{\{YBOTTOM\}}, \code{\{YTOP\}}, \code{\{XCENTER\}}, \code{\{YCENTER\}} \code{\{ID\}} and, if tiling size equal to 0
#' (processing by file), \code{\{ORIGINALFILENAME\}}.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files
#' \item \strong{drivers}: Leave it as it unless you are an advanced user.
#' \item select: The function will write file equivalent to the original ones. Thus \code{select = "*"}
#' and cannot be changed.
#' \item \strong{filter}: Read only points of interest.
#' }
