#' Read multispectral .las or .laz files
#'
#' Multispectral laser data are often stored in 3 differents files. If this is the case this function
#' reads the .las or .laz files of each channel and merges them into an object of class
#' \link[lidR:LAS-class]{LAS} and takes care of attributing an ID to each channel. If the
#' multisprectral point cloud is already stored in a single file, use \link{readLAS}. This function
#' is somewhat experimental and its names could change.
#'
#' @param files1,files2,files3 characters. Path(s) to one or several a file(s). Each argument being
#' one channel.
#' @param select,filter character. See \link{readLAS}.
#'
#' @return A LAS object
#'
#' @export
readMSLAS = function(files1, files2, files3, select = "*", filter = "")
{
  las <- readLAS(c(files1, files2, files3), select, filter)

  if (!"ScannerChannel" %in% names(las@data))
  {
    tmp <- readLAS(files1, "", filter)
    n1 <- npoints(tmp)
    tmp <- readLAS(files2, "", filter)
    n2 <- npoints(tmp)

    las@data[["ScannerChannel"]] <- 1L
    las@data[["ScannerChannel"]][(n1+1):(n1+n2)] <- 2L
    las@data[["ScannerChannel"]][(n1+n2+1):npoints(las)] <- 3L
    las@header@PHB[["Point Data Format ID"]] <- 6L
    las@header@PHB[["Version Minor"]] <- 4L
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
      las@data[["ScannerChannel"]][(n1+n2+1):npoints(las)] <- 3L
      message("ScannerChannel was not populated. The ScannerChannel of the LAS object has been automatically populated.")
    }
  }

  return(las)
}
