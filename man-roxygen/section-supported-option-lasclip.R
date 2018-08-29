#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item tiling_size: Does not make sense here
#' \item buffer: Not supported yet
#' \item alignment: Does not makes sense here
#' \item \strong{cores}: How many cores are used.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{stop_early}: Leave it as it unless you are an advanced user.
#' \item \strong{output_files}: If 'output_files' is set in the catalog, the ROIs will not be returned in R.
#' They will be written in files. See \link{LAScatalog-class} and examples. The allowed templates in
#' \code{lasclip} are \code{{XLEFT}, {XRIGHT}, {YBOTTOM}, {YTOP}, {ID}, {XCENTER},
#' {YCENTER}} or any names from the table of attributes of a \code{SpatialPolygons*} objects given as
#' input such as \code{{LAKENAME}} or \code{{YEAR}} for example if these fields exist. If empty everything
#' is returned into R.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files
#' \item \strong{drivers}: Leave it as it unless you are an advanced user.
#' \item select: The function will write file equivalent to the original ones. Thus \code{select = "*"}
#' and cannot be changed.
#' \item \strong{filter}: Read only points of interest.
#' }
