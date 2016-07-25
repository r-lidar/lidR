#' Summary of Lidar data
#'
#' This functions implements a \link[base:summary]{summary} method for Lidar objects
#'
#' @aliases summary
#' @param object An object of the class \code{Lidar}
#' @param \dots Unused (inherited from R base)
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' summary(lidar)
#'
#' @export
#' @seealso
#' \link[lidR:Lidar]{Class Lidar}
#' @importFrom utils object.size
summary.Lidar =	function(object, ...)
{
  size <- format(object.size(object), units = "auto")

  cat(paste("Memory :", size, "\n", sep=" "))

  cat("\n")

  cat("area :", object@area, "square units\n")
  cat("points :", dim(object@data)[1], "points\n")
  cat("pulses :", dplyr::n_distinct(object@data$pulseID), "pulses\n")
  cat("point density :", object@pointDensity, "points/square units\n")
  cat("pulse density :", object@pulseDensity, "pulses/square units\n")
}