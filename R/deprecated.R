#' Deprecated functions in lidR
#'
#' These functions are provided for compatibility with older versions of lidR only, and may be
#' defunct as soon as the next release.
#'
#' @param ... parameters
#' @rdname deprecated
#' @name deprecated
NULL

#' @export
#' @rdname deprecated
lasmetrics <- function(...)
{
  .Deprecated("cloud_metrics")
  return(cloud_metrics(...))
}
