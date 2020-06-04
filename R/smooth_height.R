#' Smooth a point cloud
#'
#' Point cloud-based smoothing algorithm. Two methods are available: average within a window and
#' Gaussian smooth within a window. The attribute \code{Z} of the returned LAS object is the smoothed Z.
#' A new attribute \code{Zraw} is added to store the original values and can be used to restore the
#' point cloud with \code{unsmooth_height}.
#'
#' This method does not use raster-based methods to smooth the point cloud. This is a true point cloud
#' smoothing. It is not really useful by itself but may be interesting in combination with filters such
#' as \link{filter_surfacepoints}, for example to develop new algorithms.
#'
#' @param las An object of class \code{LAS}
#' @param size numeric. The size of the windows used to smooth.
#' @param method character. Smoothing method. Can be 'average' or 'gaussian'.
#' @param shape character. The shape of the windows. Can be circle or square.
#' @param sigma numeric. The standard deviation of the gaussian if the method is gaussian.
#'
#' @return An object of the class \code{LAS}.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz")
#'
#' las <- filter_surfacepoints(las, 1)
#' plot(las)
#'
#' las <- smooth_height(las, 5, "gaussian", "circle", sigma = 2)
#' plot(las)
#'
#' las <- unsmooth_height(las)
#' plot(las)
smooth_height = function(las, size, method = c("average", "gaussian"), shape = c("circle", "square"), sigma = size/6)
{
  stopifnotlas(las)
  assert_is_a_number(size)
  assert_all_are_positive(size)
  assert_is_a_number(sigma)
  assert_all_are_positive(sigma)
  method <- match.arg(method)
  shape  <- match.arg(shape)

  if (method == "average") method <- 1  else method <- 2
  if (method == "circle") shape   <- 1  else shape  <- 2

  Zs <- C_smooth(las, size, method, shape, sigma, getThread())
  fast_quantization(Zs, las@header@PHB[["Z scale factor"]], las@header@PHB[["Z offset"]])

  if (!"Zraw" %in% names(las@data))
    las@data[["Zraw"]] <- las@data[["Z"]]

  las@data[["Z"]] <- Zs
  return(las)
}

#' @export
#' @rdname smooth_height
unsmooth_height = function(las)
{
  stopifnotlas(las)
  Z <- Zraw <- NULL

  if ("Zraw" %in% names(las@data))
  {
    las@data[["Z"]] <- las@data[["Zraw"]]
    las@data[["Zraw"]] <- NULL
  }
  else
    stop("No attribute named 'Zraw' found. Unsmoothing is not possible.")

  return(las)
}
