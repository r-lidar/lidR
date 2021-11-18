#' Convert full waveform data into a regular point cloud
#'
#' Full waveform can be difficult to manipulate and visualize in R. This function converts
#' a LAS object with full waveform data into a regular point cloud. Each waveform record
#' becomes a point with XYZ coordinates and an amplitude (units: volts) and an ID that records
#' each original pulse. Notice that this has the effect of drastically inflating the size of the
#' object in memory, which is likely already very large
#'
#' @param las An object of class LAS with full waveform data
#' @return An object of class LAS 1.2 format 0 with one point per records
#'
#' @template section-fwf
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "fwf.laz", package="rlas")
#' fwf <- readLAS(LASfile)
#' las <- interpret_waveform(fwf)
#' x <- plot(fwf, size = 3, colorPalette = "red")
#' plot(las, color = "Amplitude", bg = "white", add = x, size = 2)
#' }
interpret_waveform <- function(las)
{
  if (utils::packageVersion("rlas") <= package_version("1.4.0"))
    stop("Package rlas > 1.4.0 is required for this function")

  header <- as.list(las@header)
  data <- las@data
  fwf <- rlas::fwf_interpreter(header, data)
  fwf <- data.table::rbindlist(fwf)
  fwfheader <- rlas::header_create(fwf)
  fwfheader[["X scale factor"]] <- 1e-6
  fwfheader[["Y scale factor"]] <- 1e-6
  fwfheader[["Z scale factor"]] <- 1e-6
  fwf <- LAS(fwf, fwfheader, crs = st_crs(las), check = FALSE)
  las_quantize(fwf, by_reference = TRUE)
  fwf <- las_update(fwf)
  fwf <- add_lasattribute(fwf, name = "Amplitude", desc = "Full waveform amplitude in volts")
  return(fwf)
}
