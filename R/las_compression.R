#' Compression of the point cloud
#'
#' Package rlas 1.6.0 supports compact representation of non populated attributes. For example `UserData`
#' is usually populated with zeros (not populated). Yet it takes 32 bits per point to store each 0.
#' With rlas 1.6.0 it can now use 644 bits no matter the number of points loaded if it is not
#' populated or populated with a unique value.
#'
#' `las_is_compressed` test each attributes and returns a named vector with TRUE if the attribute is
#' compressed FALSE otherwise.\cr\cr
#' `las_size` returns the true size of a LAS object by considering the compression. `object.size` from
#' base R does not account for ALTREP and consequently cannot measure properly the size of a LAS object
#'
#' @param las A LAS object.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' las_is_compressed(las)
#'
#' format(object.size(las), units = "MB")
#' format(las_size(las), units = "MB")
#'
#' @name las_compression
#' @rdname las_compression
#' @md
NULL

#' @rdname las_compression
#' @export
las_is_compressed <- function(las)
{
  stopifnotlas(las)
  return(sapply(payload(las), is_compact))
}

#' @rdname las_compression
#' @export
las_size <- function(las)
{
  stopifnotlas(las)
  size <- 0
  gz <- las_is_compressed(las)

  for (i in seq_along(gz))
  {
    if (isTRUE(gz[i]))
      size <- size + 644L
    else
      size <- size + utils::object.size(payload(las)[[i]])
  }

  size + utils::object.size(header(las)) + utils::object.size(st_crs(las))  + utils::object.size(las@index)

  class(size) <- 'object_size'
  return(size)
}

is_compact <- function(x)
{
  altrep <- altrep_full_class(x)
  if (is.null(altrep)) return(FALSE)
  if (altrep[[2]] == "rlas") {
    if (is_materialized(x)) return(FALSE)
    return(TRUE)
  }
  return(FALSE)
}
