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
#' x <- plot(fwf, size = 3, pal = "red")
#' plot(las, color = "Amplitude", bg = "white", add = x, size = 2)
#' }
interpret_waveform <- function(las)
{
  if (utils::packageVersion("rlas") <= package_version("1.4.0"))
    stop("Package rlas > 1.4.0 is required for this function")

  # The temporal spacing is in picosecond. The light travels 300 mm per nanosecond. For a temporal
  # spacing of 1 ns two points emitted at one degree off nadir (worst case considered) are separated
  # by 300*sin(pi/180) = 5.2 mm. We need a resolution of 1 mm to store distinguishable values. With
  # a scale factor of 0.001 we are good. We use a 10th of this to be sure.
  temporal_spacing <- vlr(las)[["Full WaveForm Description"]][["Full WaveForm"]][["Temporal Spacing"]]
  one_picosecond_light <- 0.0003 # 0.3 mm
  distance_between_two_samples <- temporal_spacing * one_picosecond_light
  minimum_distance_at_one_degree_off_nadir <- distance_between_two_samples * pi/180
  s <- 10^(0:7)
  valid <- sort(c(1/s, 0.5/s))
  i <- which(valid < minimum_distance_at_one_degree_off_nadir)
  scale_factor <- valid[max(i)]/10

  header <- as.list(las@header)
  data <- las@data
  fwf <- rlas::fwf_interpreter(header, data)
  fwf <- data.table::rbindlist(fwf)
  fwfheader <- rlas::header_create(fwf)
  fwfheader[["X scale factor"]] <- scale_factor
  fwfheader[["Y scale factor"]] <- scale_factor
  fwfheader[["Z scale factor"]] <- scale_factor
  fwf <- LAS(fwf, fwfheader, crs = st_crs(las), check = FALSE)
  las_quantize(fwf, by_reference = TRUE)
  fwf <- las_update(fwf)
  fwf <- add_lasattribute(fwf, name = "Amplitude", desc = "Full waveform amplitude in volts")
  return(fwf)
}
