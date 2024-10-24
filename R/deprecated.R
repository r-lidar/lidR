#' Deprecated functions in lidR
#'
#' These functions are provided for compatibility with older versions of lidR but are deprecated.
#'
#' @param las,res See the new functions that replace the old ones
#' @param files,select,filter,sort See the new functions that replace the old ones
#' @param folder,... See the new functions that replace the old ones
#'
#' @rdname deprecated
#' @name deprecated
#'
#' @include io_readXLAS.R
#' @include io_readLAScatalog.R
NULL

# nocov start

#' @export
#' @rdname deprecated
readALSLAS = readALS

#' @export
#' @rdname deprecated
readTLSLAS = readTLS

#' @export
#' @rdname deprecated
readUAVLAS = readTLS

#' @export
#' @rdname deprecated
readDAPLAS = readTLS

#' @export
#' @rdname deprecated
readALSLAScatalog = readALScatalog

#' @export
#' @rdname deprecated
readTLSLAScatalog = readTLScatalog

#' @export
#' @rdname deprecated
readUAVLAScatalog = readTLScatalog

#' @export
#' @rdname deprecated
readDAPLAScatalog = readTLScatalog

#' @export
#' @rdname deprecated
filter_surfacepoints = function(las, res)
{
  UseMethod("filter_surfacepoints", las)
}

#' @rdname deprecated
#' @export
filter_surfacepoints.LAS = function(las, res)
{
  return(decimate_points(las, highest(res)))
}

#' @rdname deprecated
#' @export
filter_surfacepoints.LAScatalog = function(las, res)
{
  opt_select(las)       <- "*"
  opt_chunk_buffer(las) <- res
  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, filter_surfacepoints, res = res, .options = options)
  return(output)
}

.lidr3depreciation <- function(name)
{
  # no depreciation v3.0.0
  #return(invisible())

  # message v3.1.0
  #msg = paste(as.character(sys.call(sys.parent()))[1L], "is deprecated. Use", name, "instead.")
  #message(msg)
  #return(invisible())

  # warning v3.2.0
  #msg = paste(as.character(sys.call(sys.parent()))[1L], "is deprecated. Use", name, "instead.")
  #warning(msg, call. = FALSE)
  #return(invisible())

  # error v3.3.0
  msg = paste(as.character(sys.call(sys.parent()))[1L], "is defunct. Use", name, "instead.")
  stop(msg, call. = FALSE)
  return(invisible())
}

# nocov end
