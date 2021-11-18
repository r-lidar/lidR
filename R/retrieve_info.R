#' Retrieve individual pulses, flightlines or scanlines
#'
#' Retrieve each individual pulse, individual flightline or individual scanline and assigns a number
#' to each point. The LAS object must be properly populated according to LAS specifications otherwise
#' users could find unexpected outputs.
#'
#' \describe{
#' \item{\code{retrieve_pulses}}{Retrieves each individual pulse. It uses GPS time. An attribute
#' \code{pulseID} is added in the \code{LAS} object}
#' \item{\code{retrieve_scanlines}}{Retrieves each individual scanline. When data are sampled according to a
#' saw-tooth pattern (oscillating mirror), a scanline is one line, or row of data. The function relies
#' on the GPS field time to order the data. Then, the \code{ScanDirectionFlag} attribute is used to
#' retrieve each scanline. An attribute \code{scanlineID} is added in the \code{LAS} object}
#' \item{\code{retrieve_flightlines}}{Retrieves each individual flightline. It uses GPS time. In a
#' continuous dataset, once points are ordered by GPS time, the time between two consecutive points
#' does not exceed a few milliseconds. If the time between two consecutive points is too long it means
#' that the second point is from a different flightline. The default threshold is 30 seconds.
#' An attribute \code{flightlineID} is added in the \code{LAS} object.}
#' }
#'
#' @param las A LAS object
#' @param dt numeric. The threshold time-lag used to retrieve flightlines
#'
#' @return An object of class \code{LAS}
#'
#' @export
#' @rdname identify
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' las <- retrieve_pulses(las)
#' las
#'
#' las <- retrieve_flightlines(las)
#' #plot(las, color = "flightlineID")
retrieve_pulses = function(las)
{
  stopifnotlas(las)

  if (!"gpstime" %in% names(las))
    stop("No 'gpstime' attribute found. Pulse IDs cannot be computed from this object", call. = FALSE)

  if (all(las@data[["gpstime"]] == 0))
  {
    warning("'gpstime' attribute is populated with 0 only. A single ID has been returned for each point.", call. = FALSE)
    las@data[["pulseID"]] <- 1L
    return(las)
  }

  gpstime <- NULL
  data.table::setorder(las@data, gpstime)
  pulseID <- .lagisdiff(las@data[["gpstime"]])
  las@data[["pulseID"]] <- pulseID
  return(las)
}

#' @export
#' @rdname identify
retrieve_flightlines = function(las, dt = 30)
{
  stopifnotlas(las)
  assert_is_a_number(dt)
  assert_all_are_non_negative(dt)

  if (!"gpstime" %in% names(las))
    stop("No 'gpstime' attribute found. Flightlines cannot be computed from this object", call. = FALSE)

  if (all(las@data[["gpstime"]] == 0))
  {
    warning("'gpstime' attribute is populated with 0 only. A single ID has been returned for each point.", call. = FALSE)
    las@data[["flightlineID"]] <- 1L
    return(las)
  }

  gpstime <- NULL
  data.table::setorder(las@data, gpstime)
  flightlineID <- .lagissup(las@data[["gpstime"]], dt)
  las@data[["flightlineID"]] <- flightlineID
  return(las)
}

#' @export
#' @rdname identify
retrieve_scanlines = function(las)
{
  stopifnotlas(las)

  if (!"gpstime" %in% names(las))
    stop("No 'gpstime' attribute found. Scanlines IDs cannot be computed from this object", call. = FALSE)

  if (!"ScanDirectionFlag" %in% names(las))
    stop("No 'ScanDirectionFlag' attribute found. Scanlines cannot be computed from this object", call. = FALSE)

  values <- unique(las@data[["ScanDirectionFlag"]])

  if (!all(sort(values) == c(0L,1L)))
    stop("'ScanDirectionFlag' attribute is not properly populated according to LAS specifications. Scanlines IDs cannot be computed from this object'", call. = FALSE)

  gpstime <- NULL
  data.table::setorder(las@data, gpstime)
  scanlineID <- .lagisdiff(las@data[["ScanDirectionFlag"]])
  las@data[["scanlineID"]] <- scanlineID
  return(las)
}

.lagissup = function(x, dx)
{
  boo = (x - data.table::shift(x)) > dx
  boo[1] = TRUE
  return(cumsum(boo))
}

.lagisdiff = function(x)
{
  boo = x != data.table::shift(x)
  boo[1] = TRUE
  return(cumsum(boo))
}
