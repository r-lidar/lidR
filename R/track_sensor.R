#' Reconstruct the trajectory of the LiDAR sensor using multiple returns
#'
#' Use multiple returns to estimate the positioning of the sensor by computing the intersection in
#' space of the line passing through the first and last returns. To work, this function requires a
#' dataset where the 'gpstime', 'ReturnNumber', 'NumberOfReturns' and 'PointSourceID' attributes are
#' properly populated, otherwise the output may be incorrect or weird. For LAScatalog processing
#' it is recommended to use large chunks and large buffers (e.g. a swath width). The point cloud must
#' not be normalized.
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected  because it is internally known what is the
#' best to select\cr
#' The option `output_files` is not supported and not respected because the output must be post-processed
#' as a whole
#'
#' @section Test of data integrity:
#' In theory, sensor tracking is a simple problem to solve as long as each pulse is properly
#' identified from a well-populated dataset. In practice, many problems may arise from datasets that are populated
#' incorrectly. Here is a list of problems that may happen. Those with a * denote problems already encountered and
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
#' @template param-las
#' @param algorithm function. An algorithm to compute sensor tracking. \code{lidR} implements
#' \link{Roussel2020} and  \link{Gatziolis2019} (see respective documentation and examples).
#' @param extra_check boolean. Datasets are rarely perfectly populated, leading to unexpected errors.
#' Time-consuming checks of data integrity are performed. These checks can be skipped as they account
#' for an significant proportion of the computation time. See also section 'Tests of data integrity'.
#' @param thin_pulse_with_time numeric. In practice, it is not useful to compute the position using all
#' multiple returns. It is more computationally demanding but not necessarily more accurate. This keeps
#' only one pulse every x seconds. Set to 0 to use all multiple returns. Use 0 if the file has already
#' been read with \code{filter = "-thin_pulses_with_time 0.001"}.
#' @param multi_pulse logical. TRUE only for systems with multiple pulses. Pulse ID must be recorded
#' in the UserData attribute.
#' @return An sf object with POINT Z geometries. Information about the time interval and the score of
#' the positioning (according to the method used) are also in the table of attributes.
#'
#' @author Jean-Francois Bourdon & Jean-Romain Roussel
#' @export
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#' #plot(las)
#'
#' # pmin = 15 because it is an extremely small file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' flightlines <- track_sensor(las, Roussel2020(pmin = 15))
#'
#' plot(las@header)
#' plot(sf::st_geometry(flightlines), add = TRUE)
#'
#' #plot(las) |> add_flightlines3d(flightlines, radius = 10)
#'
#' \dontrun{
#' # With a LAScatalog "-drop_single" and "-thin_pulses_with_time"
#' # are used by default
#' ctg = readLAScatalog("folder/")
#' flightlines <- track_sensor(ctg,  Roussel2020(pmin = 15))
#' plot(flightlines)
#' }
#' @family range
track_sensor <- function(las, algorithm, extra_check = TRUE, thin_pulse_with_time = 0.001, multi_pulse = FALSE)
{
  UseMethod("track_sensor", las)
}

#' @export
track_sensor.LAS <- function(las, algorithm, extra_check = TRUE, thin_pulse_with_time = 0.001, multi_pulse = FALSE)
{
  if (!"PointSourceID" %in% names(las))     stop("No 'PointSourceID' attribute found", call. = FALSE)
  if (!"gpstime" %in% names(las))           stop("No 'gpstime' attribute found", call. = FALSE)
  if (!"ReturnNumber" %in% names(las))      stop("No 'ReturnNumber' attribute found", call. = FALSE)
  if (!"NumberOfReturns" %in% names(las))   stop("No 'NumberOfReturns' attribute found", call. = FALSE)
  if (!any(las@data[["gpstime"]] != 0))          stop("'gpstime' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["ReturnNumber"]] != 0))     stop("'ReturnNumber' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["NumberOfReturns"]] != 0))  stop("'NumberOfReturns' attribute is not populated.", call. = FALSE)
  if (!any(las@data[["PointSourceID"]] != 0))    stop("'PointSourceID' attribute is not populated.", call. = FALSE)

  if (multi_pulse)
  {
    if (!"UserData" %in% names(las))
      stop("No 'UserData' attribute found", call. = FALSE)
  }

  assert_is_algorithm(algorithm)
  assert_is_algorithm_trk(algorithm)
  assert_is_a_bool(extra_check)
  assert_is_a_number(thin_pulse_with_time)
  assert_all_are_non_negative(thin_pulse_with_time)
  assert_is_a_bool(multi_pulse)

  lidR.context = "track_sensor"

  . <- X <- Y <- Z <- ReturnNumber <- NumberOfReturns <- PointSourceID <- pulseID <- gpstime <- UserData <-  missing_psi <- NULL

  data <- las@data

  # Get only the first and last returns of multiple returns
  data <- data[(ReturnNumber == NumberOfReturns | ReturnNumber == 1) & NumberOfReturns > 1]

  # Filter potentially duplicated points (#396)
  if (multi_pulse == FALSE)
    dup <- duplicated(data, by = c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns"))
  else
    dup <- duplicated(data, by = c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns", "UserData"))

  if (any(dup))
  {
    warning(glue::glue("There were {sum(dup)} duplicated points. They were removed for processing."), call. = FALSE)
    data <- data[!dup]
  }

  # Decimate the dataset by gpstime
  if (thin_pulse_with_time > 0)
  {
    ftime <- round_any(data$gpstime - min(data$gpstime), thin_pulse_with_time)
    times <- data[, first(gpstime), by = ftime]$V1
    data <- data[gpstime %in% times]
  }

  # Compute an ID for each pulse
  if (multi_pulse == FALSE)
    data.table::setorder(data, gpstime, ReturnNumber)
  else
    data.table::setorder(data, UserData, gpstime, ReturnNumber) ## set record order

  data$pulseID <- .lagisdiff(data[["gpstime"]])

  # Fix #392
  if (multi_pulse)
    data[["pulseID"]] = data[["pulseID"]] + data[["UserData"]]

  # Count the points per pulse. We kept only first and last so it should be always 2
  count <- fast_table(data$pulseID,  max(data$pulseID))

  # If some pulses still have more than 2 points (#327)
  more_than_2 = fast_countover(count, 2L)
  if (more_than_2 > 0)
    stop(glue::glue("After keeping only first and last returns of multiple returns pulses, {more_than_2} pulses still have more than 2 points. This dataset is corrupted and gpstime is likely to be invalid."), call. = FALSE)

  # Some edge points might not be paired. Removed them.
  ii    <- which(count == 1L)
  data  <- data[!pulseID %in% ii]

  if (extra_check)
  {
    tests <- data[, list(test1 = .N != 2L, test2 = PointSourceID[1] != PointSourceID[2]), by = .(pulseID)]

    unpaired_pulse  <- tests$test1
    multiple_source <- tests$test2

    # Does this really happen? #327 should have made this line obsolete
    if (any(unpaired_pulse))
      warning(glue::glue("{sum(unpaired_pulse)} pulses with multiple returns were not actually paired. The point cloud is likely to be incorrectly populated. These pulses were removed"), call. = FALSE) # nocov

    # This happens if two points share the same 'gpstime' but different 'PointSourceID'
    if (any(multiple_source))
      warning(glue::glue("{sum(multiple_source)} pulses (points with same gpstime) come from different flightlines. The point cloud is likely to be incorrectly populated. These pulses were removed"), call. = FALSE)

    ii    <- tests$pulseID[unpaired_pulse | multiple_source]
    data  <- data[!pulseID %in% ii]
  }

  P <- algorithm(data)

  # If no position found return an empty sf
  if (nrow(P) == 0)
  {
    i <- integer(1)
    n <- numeric(1)
    coord <- matrix(i, ncol = 2)
    data  <- data.frame(X = coord[,1], Y = coord[, 2], Z = i, gpstime = i, PointSourceID = n, SCORE = n)
    zero  <- sf::st_as_sf(data, coords = c("X", "Y", "Z"), crs = st_crs(las))
    zero  <- zero[0,]
    return(zero)
  }
  else
  {
    data.table::setcolorder(P, c("X", "Y", "Z", "gpstime", "PointSourceID", "SCORE"))

    # Adress issue 391
    psi <- unique(data$PointSourceID)
    missing  <- !psi %in% unique(P$PointSourceID)
    if (any(missing))
    {
      missing_psi = psi[missing]
      txtids <- paste(missing_psi, collapse = ";")
      missing_psi <- as.character(missing_psi)
      warning(glue::glue("Some swaths present in the point cloud (PointSourceID = [{txtids}]) did not produce any sensor location."), call. = FALSE)
    }
  }

  na <- is.na(P[["X"]])
  P  <- P[!na]
  P  <- sf::st_as_sf(P, coords = c("X", "Y", "Z"), crs = st_crs(las))
  comment(P) <- missing_psi

  if (sum(na) > 0)
    warning(glue::glue("Something went wrong in {sum(na)} bins. The point cloud is likely to be incorrectly populated in a way not handled internally. Positions had not been computed everywere."), call. = FALSE)

  return(P)
}

#' @export
track_sensor.LAScluster <- function(las, algorithm, extra_check = TRUE, thin_pulse_with_time = 0.001, multi_pulse = FALSE)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  pos  <- track_sensor(x, algorithm, extra_check, thin_pulse_with_time, multi_pulse)
  return(pos)
}

#' @export
track_sensor.LAScatalog <- function(las, algorithm, extra_check = TRUE, thin_pulse_with_time = 0.001, multi_pulse = FALSE)
{
  assert_is_a_number(thin_pulse_with_time)
  assert_all_are_non_negative(thin_pulse_with_time)

  if (multi_pulse == FALSE)
    opt_select(las) <- "xyzrntpa"
  else
    opt_select(las) <- "xyzrntpau"

  if (thin_pulse_with_time > 0)
    opt_filter(las) <- paste("-drop_single -thin_pulses_with_time", format(thin_pulse_with_time, scientific =  FALSE), opt_filter(las))
  else
    opt_filter(las) <- paste("-drop_single", opt_filter(las))

  if (opt_output_files(las) != "")
  {
    opt_output_files(las) <- ""
    warning("Saving intermediate results is disabled in 'track_sensor' because the output must be post-processed as a whole.", call. = F)
  }

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = FALSE)
  output  <- catalog_apply(las, track_sensor, algorithm = algorithm, extra_check = extra_check, thin_pulse_with_time = 0, multi_pulse = multi_pulse, .options = options)
  output  <- do.call(rbind, output)

  # Post-processing to remove duplicated positions selecting the one computed with the more pulses
  SCORE <- gpstime <- NULL
  data <- sf::st_drop_geometry(output)
  data.table::setDT(data)
  i <- data[, .I[which.max(SCORE)], by = gpstime]$V1
  data.table::setDF(data)

  return(output[i,])
}
