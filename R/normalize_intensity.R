#' Normalize intensity
#'
#' Normalize intensity values using multiple methods.
#'
#' @template param-las
#' @param algorithm an intensity normalizaton algorithm. \code{lidR} currently has \link{range_correction}.
#' @return Returns an object of class LAS. The attribute 'Intensity'
#' records the normalized intensity. An extra attribute named 'RawIntensity' records the original
#' intensities.
#'
#' @template LAScatalog
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[=LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk size}: How much data is loaded at once.
#' \item chunk buffer: No buffer needed. A buffer of 0 is used and cannot be changed
#' \item \strong{chunk alignment}: Align the processed chunks.
#' \item \strong{progress}: Displays a progression estimation.
#' \item \strong{output files*}: Mandatory because the output is likely to be too big to be returned
#' in R and needs to be written in las/laz files. Supported templates are \code{\{XLEFT\}}, \code{\{XRIGHT\}},
#' \code{\{YBOTTOM\}}, \code{\{YTOP\}}, \code{\{XCENTER\}}, \code{\{YCENTER\}} \code{\{ID\}} and, if
#' chunk size is equal to 0 (processing by file), \code{\{ORIGINALFILENAME\}}.
#' \item select: The function will write files equivalent to the original ones. Thus \code{select = "*"}
#' and cannot be changed.
#' \item \strong{filter}: Read only points of interest.
#' }
#'
#' @export
#'
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # pmin = 15 because it is an extremely small file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' sensor <- track_sensor(las, Roussel2020(pmin = 15))
#'
#' # Here the effect is virtually null because the size of
#' # the sample is too small to notice any effect of range
#' las <- normalize_intensity(las, range_correction(sensor, Rs = 2000))
#' @family normalize
normalize_intensity <- function(las, algorithm)
{
  UseMethod("normalize_intensity", las)
}

#' @export
normalize_intensity.LAS <- function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_nit(algorithm)
  lidR.context <- "normalize_intensity"
  intensity <- algorithm(las)
  invalid   <- fast_countequal(intensity, 65535)

  if (invalid > 0)
    warning(glue::glue("{invalid} points have a normalized intensity greater than 65535. Intensity replaced by 65535"), call. = FALSE)

  las@data[["RawIntensity"]] <- las@data[["Intensity"]]
  las@data[["Intensity"]]    <- intensity

  return(las)
}

#' @export
normalize_intensity.LAScluster = function(las, algorithm)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- normalize_intensity(x, algorithm)
  return(x)
}

#' @export
normalize_intensity.LAScatalog = function(las, algorithm)
{
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_sapply(las, normalize_intensity, algorithm = algorithm, .options = options)
  return(output)
}
