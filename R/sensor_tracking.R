#' Reconstruct the trajectory of the LiDAR sensor using multiple returns
#'
#' Use multiple returns to estimate the positioning of the sensor by computing the intersection in
#' space of the line passing through the first and last returns. To work, this function requires a
#' dataset where the 'gpstime', 'ReturnNumber', 'NumberOfReturns' and 'PointSourceID' attributes are
#' properly populated, otherwise the output may be incorrect or weird. For LAScatalog processing
#' it is recommended to use large chunks and large buffers (e.g. a swath width). The point cloud must
#' not be normalized.
#'
#' When multiple returns from a single pulse are detected, the sensor computes their positions as being
#' in the center of the footprint and thus all aligned. Because of that behavior, a line
#' drawn between and beyond those returns must cross the sensor. Thus, several consecutive pulses
#' emitted in a tight interval (e.g. 0.5 seconds) can be used to approximate an intersection
#' point in the sky that corresponds to the sensor position given that the sensor carrier hasn't
#' moved much during this interval. A weighted least squares method gives an approximation of the
#' intersection by minimizing the squared sum of the distances between the intersection point and all
#' the lines.
#'
#' @section Test of data integrity:
#' In theory, sensor tracking is a simple problem to solve as long as each pulse is properly
#' identified from a well-populated dataset. In practice, many problems may arise from datasets that are populated
#' incorrectly. Here is a list of problem that may happen. Those with a * denote problems already encountered and
#' internally checked to remove weird points:
#' \itemize{
#' \item 'gpstime' does not record the time at which pulses were emitted and thus pulses are not identifiable
#' \item *A pulse (two or more points that share the same gpstime) is made of points from different
#' flightlines (different PointSourceID). This is impossible and denotes an improperly populated PointSourceID
#' attribute.
#' \item 'ReturnNumber' and 'NumberOfReturns' are wrongly populated with either some ReturnNumber > NumberOfReturn
#'  or several first returns by pulses
#' }
#' For a given time interval, when weird points are not filtered, the position is not computed for this
#' interval.
#'
#' @template LAScatalog
#' @template section-supported-options-sensor_tracking
#'
#' @template param-las
#' @param interval numeric. Interval used to bin the gps times and group the pulses to compute
#' a position at a given timepoint t.
#' @param pmin integer. Minimum number of pulses needed to estimate a sensor position.
#' For a given interval, the sensor position is not computed if the number of pulses is lower than
#' \code{pmin}.
#' @param extra_check boolean. Datasets are rarely perfectly populated, leading to unexpected errors.
#' Time-consuming checks of data integrity are performed. These checks can be skipped as they account
#' for an important proportion of the computation time. See also section 'Tests of data integrity'.
#' @param thin_pulse_with_time numeric. In practice, it is useless to compute the position using all
#' multiple returns. It is more computationally demanding but not necessarily more accurate. This keeps
#' only one pulse every x seconds. Set to 0 to use all multiple returns. Use 0 if the file has already
#' been read with \code{filter = "-thin_pulses_with_time 0.001"}.
#' @return A SpatialPointsDataFrame with the Z elevation stored in the table of attributes. Information
#' about the time interval and the number of pulses used to find the points is also in the table of
#' attributes.
#'
#' @author Jean-Francois Bourdon & Jean-Romain Roussel
#' @export
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#' plot(las)
#'
#' # pmin = 15 because it is an extremely tiny file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' flightlines <- sensor_tracking(las, pmin = 15)
#'
#' plot(las@header)
#' plot(flightlines, add = TRUE)
#'
#' x <- plot(las)
#' add_flightlines3d(x, flightlines, radius = 10)
#'
#' # Load only the data actually useful
#' las <- readLAS(LASfile,
#'                select = "xyzrntp",
#'                filter = "-drop_single -thin_pulses_with_time 0.001")
#' flightlines <- sensor_tracking(las)
#'
#' x <- plot(las)
#' add_flightlines3d(x, flightlines, radius = 10)
#'
#' \dontrun{
#' # With a LAScatalog "-drop_single" and "-thin_pulses_with_time"
#' # are used by default
#' ctg = readLAScatalog("folder/")
#' flightlines <- sensor_tracking(ctg)
#' plot(flightlines)
#' }
sensor_tracking <- function(las, interval = 0.5, pmin = 50, extra_check = TRUE, thin_pulse_with_time = 0.001)
{
  UseMethod("sensor_tracking", las)
}

#' @export
sensor_tracking.LAS <- function(las, interval = 0.5, pmin = 50, extra_check = TRUE, thin_pulse_with_time = 0.001)
{
  if (!"PointSourceID" %in% names(las@data))     stop("No 'PointSourceID' attribute found", call. = FALSE)
  if (!"gpstime" %in% names(las@data))           stop("No 'gpstime' attribute found", call. = FALSE)
  if (!"ReturnNumber" %in% names(las@data))      stop("No 'ReturnNumber' attribute found", call. = FALSE)
  if (!"NumberOfReturns" %in% names(las@data))   stop("No 'NumberOfReturns' attribute found", call. = FALSE)
  if (!any(las@data[["gpstime"]] != 0))          stop("'gpstime' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["ReturnNumber"]] != 0))     stop("'ReturnNumber' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["NumberOfReturns"]] != 0))  stop("'NumberOfReturns' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["PointSourceID"]] != 0))    stop("'PointSourceID' attribute is not populated.", call. = FALSE)

  assert_is_a_number(interval)
  assert_is_a_number(pmin)
  assert_is_a_bool(extra_check)
  assert_is_a_number(thin_pulse_with_time)

  . <- X <- Y <- Z <- ReturnNumber <- NumberOfReturns <- PointSourceID <- pulseID <- gpstime <-  NULL

  data <- las@data

  # Get only the first and last returns of multiple returns
  data <- data[(ReturnNumber == NumberOfReturns | ReturnNumber == 1) & NumberOfReturns > 1]

  # Decimate the dataset by gpstime
  if (thin_pulse_with_time > 0)
  {
    ftime <- round_any(data$gpstime, thin_pulse_with_time)
    times <- data[, first(gpstime), by = ftime]$V1
    data <- data[gpstime %in% times]
  }

  # Compute an ID for each pulse
  data.table::setorder(data, gpstime, ReturnNumber)
  data$pulseID <- .lagisdiff(data[["gpstime"]])

  # Count the point per pulse. We kept only first and last so it should be always 2
  count <- fast_table(data$pulseID,  max(data$pulseID))

  # If some pulses still have more than 2 points (#327)
  more_than_2 = fast_countover(count, 2L)
  if (more_than_2 > 0)
    stop(glue::glue("After keeping only first and last returns of multiple returns pulses, {more_than_2} pulses still have more than 2 points. This dataset is corrupted and gpstime is likely to be invalid."), call. = FALSE)

  # Some edge point might no be paired. Removed them.
  ii    <- which(count == 1L)
  data  <- data[!pulseID %in% ii]

  if (extra_check)
  {
    tests <- data[, list(test1 = .N != 2L, test2 = PointSourceID[1] != PointSourceID[2]), by = .(pulseID)]

    unpaired_pulse  <- tests$test1
    multiple_source <- tests$test2

    # Does this really happen? #327 should have made this line obsolete
    if (any(unpaired_pulse))
      warning(glue::glue("{sum(unpaired_pulse)} pulses with multiple returns were not actually paired. The point cloud is likely to be wrongly populated. These pulses were removed"), call. = FALSE) # nocov

    # This happens if two points share the same 'gpstime' but different 'PointSourceID'
    if (any(multiple_source))
      warning(glue::glue("{sum(multiple_source)} pulses (points with same gpstime) come from different flightlines. The point cloud is likely to be wrongly populated. These pulses were removed"), call. = FALSE)

    ii    <- tests$pulseID[unpaired_pulse | multiple_source]
    data  <- data[!pulseID %in% ii]
  }

  # Generate the bins
  bins <- round_any(data$gpstime, interval)

  # Find the position P of the sensor in each interval
  P  <- data[, if (.N > 2*pmin) sensor_positions(X,Y,Z, ReturnNumber), by = .(gpstime = bins, PointSourceID = PointSourceID)]

  # If no position found return an empty SpatialPointsDataFrame
  if (nrow(P) == 0)
  {
    i <- integer(1)
    n <- numeric(1)
    coord <- matrix(i, ncol = 2)
    data  <- data.frame(Z = i, gpstime = i, PointSourceID = n, npulses = n)
    zero  <- sp::SpatialPointsDataFrame(coord, data, proj4string = las@proj4string)
    zero  <- zero[-1,]
    return(zero)
  }

  na <- is.na(P[["X"]])
  P  <- P[!na]
  P  <- sp::SpatialPointsDataFrame(P[,3:4], P[,c(5,1,2,6)], proj4string = las@proj4string)

  if (sum(na) > 0)
    warning(glue::glue("Something went wrong in {sum(na)} bins. The point cloud is likely to be wrongly populated in a way not handled internally. Positions had not been computed everywere."), call. = FALSE)

  return(P)
}

#' @export
sensor_tracking.LAScluster <- function(las, interval = 0.5, pmin = 50, extra_check = TRUE, thin_pulse_with_time = 0.001)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  pos  <- sensor_tracking(x, interval, pmin, extra_check)
  return(pos)
}

#' @export
sensor_tracking.LAScatalog <- function(las, interval = 0.5, pmin = 50, extra_check = TRUE, thin_pulse_with_time = 0.001)
{
  assert_is_a_number(thin_pulse_with_time)

  opt_select(las) <- "xyzrntp"
  opt_filter(las) <- paste("-drop_single -thin_pulses_with_time", format(thin_pulse_with_time, scientific =  FALSE), opt_filter(las))

  if (opt_output_files(las) != "")
  {
    opt_output_files(las) <- ""
    warning("Saving intermediate results is disabled in 'sensor_tracking' because the output must be post-processed as a whole.", call. = F)
  }

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = FALSE)
  output  <- catalog_apply(las, sensor_tracking, interval = interval, pmin = pmin, extra_check = extra_check, thin_pulse_with_time = 0, .options = options)
  output  <- do.call(rbind, output)
  output@proj4string <- las@proj4string

  # Post-processing to remove duplicated positions selecting the one computed with the more pulses
  npulses <- gpstime <- NULL
  data <- as.data.frame(output)
  data.table::setDT(data)
  i <- data[, .I[which.max(npulses)], by = gpstime]$V1

  return(output[i,])
}

sensor_positions <- function(x, y, z, rn)
{
  first <- rn == 1L
  last  <- rn > 1L

  # Adapted from MATLAB (Anders Eikenes, 2012)
  # http://www.mathworks.com/matlabcentral/fileexchange/37192-intersection-point-of-lines-in-3d-space

  # Matrix n x 3 containing XYZ coordinates for each first/last return
  A <- matrix(c(x[first], y[first], z[first]), ncol = 3L)
  B <- matrix(c(x[last], y[last], z[last]), ncol = 3L)

  # This should never happen but it might happen in wrongly populated datasets
  if (nrow(A) != nrow(B))
    return(list(X = NA_real_, Y = NA_real_, Z = NA_real_, npulses = NA_integer_))

  # Lines described as vectors V
  V <- B - A

  # Weights lines by distances
  D <- matrix(sqrt(.rowSums(V^2, nrow(V), ncol(V))))
  W <- D / sum(D)

  # Normalized vectors (|V| = 1)
  ones <- matrix(1, ncol = 3)
  N  <- V / (D %*% ones)
  Nx <- N[, 1L, drop = FALSE]
  Ny <- N[, 2L, drop = FALSE]
  Nz <- N[, 3L, drop = FALSE]

  Wxx <- W * (Nx^2 - 1)
  Wyy <- W * (Ny^2 - 1)
  Wzz <- W * (Nz^2 - 1)
  Wxy <- W * Nx * Ny
  Wxz <- W * Nx * Nz
  Wyz <- W * Ny * Nz

  Sxx <- sum(Wxx)
  Syy <- sum(Wyy)
  Szz <- sum(Wzz)
  Sxy <- sum(Wxy)
  Sxz <- sum(Wxz)
  Syz <- sum(Wyz)
  S   <- matrix(c(Sxx, Sxy, Sxz, Sxy, Syy, Syz, Sxz, Syz, Szz), nrow = 3L)

  Cx <- sum(A[,1, drop = FALSE] * Wxx + A[,2, drop = FALSE] * Wxy + A[,3, drop = FALSE] * Wxz)
  Cy <- sum(A[,1, drop = FALSE] * Wxy + A[,2, drop = FALSE] * Wyy + A[,3, drop = FALSE] * Wyz)
  Cz <- sum(A[,1, drop = FALSE] * Wxz + A[,2, drop = FALSE] * Wyz + A[,3, drop = FALSE] * Wzz)
  C  <- matrix(c(Cx,Cy,Cz))

  M  <- t(solve(S,C))

  return(list(X = M[1], Y = M[2], Z = M[3], npulses = nrow(A)))
}
