#' Smooth a point cloud
#'
#' Point cloud-based smoothing algorithm. Two methods are available: average within a window and
#' Gaussian smooth within a window. The original object is updated in place. The Z column is now
#' the smoothed Z. A new column Zraw is added to store the original values and can be used to
#' restore the point cloud with \code{lasunsmooth}.
#'
#' This method does not use raster-based methods to smooth the point cloud. This is a true point cloud
#' smoothing. It is not really useful by itself but may be interesting in combination with filters such
#' as \link{lasfiltersurfacepoints}, for example.
#'
#' @param las An object of class \code{LAS}
#' @param size numeric. The size of the windows used to smooth
#' @param method character. Smoothing method. Can be 'average' or 'gaussian'.
#' @param shape character. The shape of the windows. Can be circle or square.
#' @param sigma numeric. The standard deviation of the gaussian if the method is gaussian
#'
#' @return Nothing (NULL). The original object has been updated in place. The 'Z' column is now the
#' smoothed 'Z'. A new column 'Zraw' is added in the original object to store the original values.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' las = lasfiltersurfacepoints(las, 1)
#' plot(las)
#'
#' lassmooth(las, 5, "gaussian", "circle", sigma = 2)
#' plot(las)
#'
#' lasunsmooth(las)
#' plot(las)
#' @seealso \link{lasfiltersurfacepoints}
lassmooth = function(las, size, method = c("average", "gaussian"), shape = c("circle", "square"), sigma = size/6)
{
  stopifnotlas(las)
  assertive::assert_is_a_number(size)
  assertive::assert_all_are_positive(size)
  assertive::assert_is_a_number(sigma)
  assertive::assert_all_are_positive(sigma)
  method <- match.arg(method)
  shape  <- match.arg(shape)

  Z <- Zraw <- NULL

  if (method == "average") method = 1  else method = 2
  if (method == "circle") shape = 1 else shape = 2

  Zs = C_lassmooth(las, size, method, shape, sigma)

  if (!"Zraw" %in% names(las@data))
    las@data[, Zraw := Z]

  las@data[, Z := Zs]
}

#' @export
#' @rdname lassmooth
lasunsmooth = function(las)
{
  stopifnotlas(las)
  Z <- Zraw <- NULL

  if ("Zraw" %in% names(las@data))
  {
    las@data[, Z := Zraw]
    las@data[, Zraw := NULL]
    las@data[]
  }
  else
    message("No column named 'Zraw' found. Unsmoothing is not possible.")
}