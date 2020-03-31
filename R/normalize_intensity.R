#' Normalize intensity
#'
#' Normalize intensity value using multiple methods.
#'
#' @template param-las
#' @param algorithm an intensity normalizaton algorithm. \code{lidR} currently have \link{range_correction}.
#' @return Returns an object of class LAS. The attribute 'Intensity'
#' records the normalised intensity. An extra attribute named 'RawIntensity' records the original
#' intensities.
#'
#' @export
#'
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # pmin = 15 because it is an extremely tiny file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' sensor <- track_sensor(las, pmin = 15)
#'
#' # Here the effect is virtually null because the size of
#' # the sample is too tiny to notice any effect of range
#' las <- normalize_intensity(las, range_correction(sensor, Rs = 2000))
#' @family normalize
normalize_intensity <- function(las, algorithm)
{
  UseMethod("normalize_intensity", las)
}

#' @export
normalize_intensity.LAS <- function(las, algorithm)
{
  lidR.context = "normalize_intensity"
  intensity <- algorithm(las)
  invalid   <- fast_countequal(intensity, 65535)

  if (invalid > 0)
    warning(glue::glue("{invalid} points have a normalized intensity greater than 65535. Intensity replaced by 65535"), call. = FALSE)

  las@data[["RawIntensity"]] <- las@data[["Intensity"]]
  las@data[["Intensity"]]    <- intensity

  return(las)
}
