#' Deprecated functions in lidR
#'
#' These functions are provided for compatibility with older versions of lidR but are deprecated. They
#' will progressively print a message, throw a warning and eventually be removed. The links below point
#' to the documentation of the new names. In version 4 they now throw an error. In version 4.1 they
#' ill be removed definitively.\cr\cr
#' \link[=add_attribute]{lasadd} \link[=las_check]{lascheck} \link[=clip]{lasclip}
#' \link[=segment_shapes]{lasdetectshape} \link[=filter_poi]{lasfilter}
#' \link[=filter_surfacepoints]{lasfiltersurfacepoints} \link[=retrieve_flightlines]{lasflightline}
#' \link[=classify_ground]{lasground} \link[=merge_spatial]{lasmergespatial}
#' \link[=normalize_height]{lasnormalize} \link[=retrieve_pulses]{laspulse}
#' \link[=normalize_intensity]{lasrangecorrection} \link[=retrieve_flightlines]{lasflightline}
#' \link[=las_reoffset]{lasreoffset} \link[=las_rescale]{lasrescale}
#' \link[=retrieve_scanlines]{lasscanlines} \link[=smooth_height]{lassmooth}
#' \link[=segment_snags]{lassnags}
#' \link[=segment_trees]{lastrees} \link[=voxelize_points]{lasvoxelize}
#' \link[=track_sensor]{sensor_tracking} \link[=locate_trees]{tree_detection}
#' \link[=crown_metrics]{tree_hull}
#'
#' @param las,res See the new functions that replace the old ones
#' @param files,select,sort See the new functions that replace the old ones
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
#' @rdname readLAS
readDAPLAS = readTLS

#' @export
#' @rdname readLAScatalog
readALSLAScatalog = readALScatalog

#' @export
#' @rdname readLAScatalog
readTLSLAScatalog = readTLScatalog

#' @export
#' @rdname readLAScatalog
readUAVLAScatalog = readTLScatalog

#' @export
#' @rdname readLAScatalog
readDAPLAScatalog = readTLScatalog

#' @export
#' @rdname deprecated
hexbin_metrics = function(...)
{
  .lidr3depreciation("hexagon_metrics")
}

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
