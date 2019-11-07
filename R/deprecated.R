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

#' @export
#' @rdname deprecated
grid_metrics3d <- function(...) {
  .Deprecated("voxel_metrics")
  return(voxel_metrics(...))
}

#' @export
#' @rdname deprecated
grid_hexametrics <- function(...) {
  .Deprecated("hexbin_metrics")
  return(hexbin_metrics(...))
}
