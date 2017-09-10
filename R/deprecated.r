#' Deprecated function(s) in the lidR package
#'
#' These functions are provided for compatibility with older version of
#' the lidR package.  They may eventually be completely
#' removed.
#' @rdname lidR-deprecated
#' @name lidR-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export
#' @section Details:
#' \itemize{
#' \item{\code{\link{lasmetrics}} replace old \code{cloud_metrics}}
#' \item{\code{\link{area}} replace old \code{lasarea}}
#' }
#'
cloud_metrics = function(...)
{
  .Deprecated("lasmetrics", package="lidR")
  lasmetrics(...)
}

#' @export
#' @rdname lidR-deprecated
#' @name lidR-deprecated
lasarea = function(...)
{
  .Deprecated("lasarea", package="lidR")
  area(...)
}
NULL