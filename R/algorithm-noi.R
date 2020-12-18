#' Intensity normalization algorithm
#'
#' This function is made to be used in \link{normalize_intensity}. It corrects intensity with a
#' range correction according to the formula (see references):
#' \deqn{I_{norm} = I_{obs} (\frac{R}{Rs})^f)}{Inorm = Iobs * (R/Rs)^f}
#' To achieve the range correction the position of the sensor must be known at different discrete times.
#' Using the 'gpstime' of each point, the position of the sensor is interpolated from the reference
#' and a range correction is applied.
#'
#' @param sensor \code{SpatialPointsDataDrame} object containing the coordinates of
#' the sensor at different timepoints t. The time and elevation are stored as attributes
#' (default names are 'gpstime' and 'Z'). It can be computed with \link{track_sensor}.
#' @param Rs numeric. Range of reference.
#' @param f numeric. Exponent. Usually between 2 and 3 in vegetation contexts.
#' @param gpstime,elevation character. The name of the attributes that store the gpstime of the
#' position and the elevation of the sensor respectively. If \code{elevation = NULL} the Z coordinates
#' are searched in the third column of the coordinates matrix of the SpatialPointsDataFrame. This is
#' useful if read from a format that supports 3 coordinates points.
#' @param las an object of class LAS. \code{get_range()} is a regular function documented here for
#' convenience.
#' @export
#' @references
#' Gatziolis, D. (2011). Dynamic Range-based Intensity Normalization for Airborne, Discrete Return
#' Lidar Data of Forest Canopies. Photogrammetric Engineering & Remote Sensing, 77(3), 251–259.
#' https://doi.org/10.14358/pers.77.3.251
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # pmin = 15 because it is an extremely tiny file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' sensor <- track_sensor(las, Roussel2020(pmin = 15))
#'
#' # Here the effect is virtually null because the size of
#' # the sample is too small to notice any effect of range
#' las <- normalize_intensity(las, range_correction(sensor, Rs = 2000))
#'
#' # This might be useful for some applications
#' R = get_range(las, sensor)
range_correction = function(sensor, Rs, f = 2.3, gpstime = "gpstime", elevation = "Z")
{
  assert_is_a_number(Rs)
  assert_is_a_number(f)
  assert_all_are_positive(Rs)
  assert_all_are_positive(f)
  assert_is_all_of(sensor, "SpatialPointsDataFrame")

  Rs <- lazyeval::uq(Rs)
  f <- lazyeval::uq(f)
  gpstime <- lazyeval::uq(gpstime)
  elevation <- lazyeval::uq(elevation)

  fun = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTNIT, "range_correction")
    fl <- prepare_sensor(sensor, gpstime, elevation, las)
    return(C_lasrangecorrection(las, fl, Rs, f))
  }

  class(fun) <- LIDRALGORITHMNIT
  return(fun)
}

#' @rdname range_correction
#' @export
get_range = function(las, sensor, gpstime = "gpstime", elevation = "Z")
{
  stopifnotlas(las)
  fl <- prepare_sensor(sensor, gpstime, elevation, las)
  R <- C_lasrange(las, fl)
  return(round(R, 3))
}

prepare_sensor = function(sensor, gpstime, elevation, las)
{
  stopifnot(is(sensor, "SpatialPointsDataFrame"))

  if (!"gpstime"   %in% names(las@data))
    stop("No 'gpstime' attribute found in las",   call. = FALSE)

  if (!"Intensity" %in% names(las@data))
    stop("No 'Intensity' attribute found", call. = FALSE)

  # Relaxed in #395
  #if (is.null(raster::intersect(raster::extent(las), raster::extent(sensor))))
  #  stop("Point-cloud and sensor positions do not overlap.", call. = FALSE)

  if (!gpstime %in% names(sensor@data))
    stop(glue::glue("No '{gpstime}' attribute found in sensor."), call. = FALSE)

  if (ncol(sensor@coords) == 3 && !is.null(elevation))
  {
    elevation = NULL
    message("3 coordinates detected in the sensor positions, parameter 'elevation' was not considered.")
  }

  fl <- sensor@coords
  fl <- data.table::data.table(fl)

  trange.las <- range(las@data[["gpstime"]])
  trange.sensor <- range(sensor@data[[gpstime]])

  if ((trange.las[1] < trange.sensor[1] - 2) || (trange.las[2] > trange.sensor[2] + 2))
    stop("'gpstime range from the sensor does not contain gpstime range from the point-cloud", call. = FALSE)

  if (!is.null(elevation)) {
    if (!elevation %in% names(sensor@data))
      stop(glue::glue("No '{elevation}' attribute found in sensor."), call. = FALSE)

    fl[["Z"]] <- sensor@data[[elevation]]
  } else {
    if (ncol(sensor@coords) != 3)
      stop("There are only two dimensions in the coordinates of the sensor.", call. = FALSE)
  }

  fl[["gpstime"]] <- sensor@data[[gpstime]]

  fl <- data.table::data.table(fl)
  data.table::setorder(fl, gpstime)
  data.table::setnames(fl, c("X", "Y", "Z", "gpstime"))
  dup <- duplicated(fl, by = "gpstime")

  if (any(dup)) {
    warning("Duplicated gpstime found. Duplicated sensor positions were removed.", call. = FALSE)
    fl <- fl[!dup]
  }

  return(fl)
}

