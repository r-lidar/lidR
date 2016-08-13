#' Load a las file and create a 'LAS' object
#'
#' Methods to read and create a \code{LAS} object from a vector of .las filename(s)
#'
#' Methods to read and create a \code{LAS} object from a vector of .las filename(s).
#' The option fields allows selection of fields to be loaded. Removing redundant fields
#' saves memory. The option '\code{minimal}' loads only X,Y,Z and gpstime allowing
#' pulseID and flightlineID to be computed. The option '\code{standard}' loads all fields
#' apart from UserDate, EdgeofFlighline and PointSourceID. The option '\code{all}' loads everything.
#'
#' @param input character or Catalog object. Filename of .las file. Use \link[base:c]{c()} to concatenate several files.
#' If input is a \link[lidR:Catalog-class]{Catalog} object, all the .las file in the catalog will be loaded.
#' @param fields character. Can be \code{"minimal"}, \code{"standard"}, \code{"all"}. Default is standard. See details.
#' @param \dots Unused
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' getData(lidar)
#' summary(lidar)
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' \link[lidR:getData]{getData}
#' \link[lidR:summary]{summary}
#' @export readLAS
LAS <- function(input, fields = "standard", ...) {return(new("LAS", input, fields, ...))}
