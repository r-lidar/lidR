#' @export
#' @rdname readLAS
readALS = function(files,  select = "*", filter = "")
{
  do_no_read_lascluster(files)
  las <- readLAS(files, select, filter)
  las@index <- LIDRALSINDEX
  return(las)
}


#' @export
#' @rdname readLAS
#' @param sort boolean. To optimize even more the computation speed the point cloud is spatially sorted
readTLS = function(files,  select = "*", filter = "", sort = TRUE)
{
  do_no_read_lascluster(files)
  las <- readLAS(files, select, filter)
  las@index <- LIDRTLSINDEX
  if (sort) tls_spatial_sort(las)
  return(las)
}

#' @section Multispectral data:
#' Multispectral laser data are often stored in 3 different files. If this is the case
#' \code{readMSLAS} reads the .las or .laz files of each channel and merges them into
#' an object of class \link[=LAS-class]{LAS} and takes care of attributing an ID to each
#' channel. If the multisprectral point cloud is already stored in a single file
#' leave \code{file2} and \code{file3} missing.
#'
#' @param files1,files2,files3 characters. Path(s) to one or several a file(s). Each argument being
#' one channel (see section 'Multispectral data'). `files2` and `files3` can be missing.
#'
#' @export
#' @rdname readLAS
readMSLAS = function(files1, files2, files3, select = "*", filter = "")
{
  if (missing(files2) && missing(files3))
  {
    las <- readLAS(files1, select, filter)
    las@index <- LIDRMLSINDEX
    return(las)
  }

  if (missing(files3)) files3 <- NULL

  las <- readLAS(c(files1, files2, files3), select, filter)

  if ("ScanAngleRank" %in% names(las))
  {
    data.table::setnames(las@data, "ScanAngleRank", "ScanAngle")
    las@data[["ScanAngle"]] <- as.numeric(las@data[["ScanAngle"]])
  }

  if (!"ScannerChannel" %in% names(las))
  {
    tmp <- readLAS(files1, "", filter)
    n1 <- npoints(tmp)
    tmp <- readLAS(files2, "", filter)
    n2 <- npoints(tmp)

    las@data[["ScannerChannel"]] <- 1L
    las@data[["ScannerChannel"]][(n1+1):(n1+n2)] <- 2L
    if (!is.null(files3))
      las@data[["ScannerChannel"]][(n1+n2+1):npoints(las)] <- 3L

    las@header@PHB[["Point Data Format ID"]] <- 6L
    las@header@PHB[["Version Minor"]] <- 4L
    las@header@PHB[["Header Size"]] <- 375L
    message("Multispectral data read from point format < 6 with no ScannerChannel attribute. The LAS object has been upgraded to LAS 1.4 prf 6 with a ScannerChannel.")
  }
  else
  {
    n <- fast_countequal(las@data[["ScannerChannel"]], 0L)

    if (n > 0 && n < npoints(las))
      warning("Some points have a ScannerChannel of 0 meaning they come from single source sensor.", call. = FALSE)

    if (n == npoints(las)) {
      tmp <- readLAS(files1, "", filter)
      n1 <- npoints(tmp)
      tmp <- readLAS(files2, "", filter)
      n2 <- npoints(tmp)

      las@data[["ScannerChannel"]] <- 1L
      las@data[["ScannerChannel"]][(n1+1):(n1+n2)] <- 2L
      if (!is.null(files3))
        las@data[["ScannerChannel"]][(n1+n2+1):npoints(las)] <- 3L

      message("ScannerChannel was not populated. The ScannerChannel of the LAS object has been automatically populated.")
    }
  }

  las@index <- LIDRMLSINDEX
  return(las)
}

tls_spatial_sort = function(las)
{
  gpstime <- NULL

  las@data[["order"]] = C_voxel_id(las, 0.1)

  if ("gpstime" %in% names(las))
    data.table::setorder(las@data, order, gpstime)
  else
    data.table::setorder(las@data, order)

  las@data[, order := NULL]
  return(invisible())
}

do_no_read_lascluster = function(x)
{
  if (is(x, "LAScluster"))
    stop("Use readLAS() to read a LAScluster object not one of readXXXLAS(). A LAScluster object already contains metadata about the point cloud type.", call. = FALSE)
}
