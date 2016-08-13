#' Thin LiDAR data
#'
#' Thin LIDAR data randomly removes a given proportion of pulses to reach specific pulse densities
#'
#' Thin is designed to produce output data sets that have uniform pulse densities
#' throughout the coverage area. For each cell, the proportion of pulses that will
#' be retained is computed using the calculated pulse density and the desired pulse
#' density. If required pulse density is greater than the local pulse density it returns
#' an unchanged set of points (it cannot increase the pulse density). In the way
#' of \code{homogenize = FALSE} it randomly removes pulses to reach the required pulse
#' density on the whole area (see \code{\link[lidR:area]{area}}). The cell size must be large enough
#' to compute a coherant local pulse density. 25 square meters looks good. 1 square
#' meter is meaningless.
#' @aliases  thin
#' @param obj An object of the class \code{LAS}
#' @param pulseDensity numeric. The expected pulseDensity
#' @param homogenize logical. If \code{TRUE}, the algorithm tries to homogenize the pulse density to provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density on the whole area.
#' @param resolution numeric. Cell size to compute the pulse density.
#' @return It returns a \code{LAS} object.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # By default the method is homogenize = TRUE
#' thinned = lidar %>% thin(1, resolution = 5)
#' lidar   %>% pulseDensity %>% plot
#' thinned %>% pulseDensity %>% plot
#'
#' # Method homogenize = FALSE enables a global pulse density to be reached
#' thinned = lidar %>% thin(1, homogenize = FALSE)
#' thinned %>% summary
#' thinned %>% pulseDensity %>% plot
#' @export thin
#' @importFrom plyr round_any
#' @importFrom dplyr n_distinct
#' @importFrom data.table := setnames setorder
setGeneric("thin", function(obj, pulseDensity, homogenize = TRUE, resolution = 5){standardGeneric("thin")})

#' @rdname thin
setMethod("thin", c("LAS", "numeric"),
	function(obj, pulseDensity, homogenize = TRUE, resolution = 5)
  {
	  pulseID <- gpstime <- NULL

    if(homogenize == FALSE)
    {
      n = round(pulseDensity*obj@area)
      selected = .selectPulseToRemove(obj@data$pulseID, n)
    }
    else
    {
      n = round(pulseDensity*resolution^2)

      x_raster = plyr::round_any(obj@data$X, resolution)
      y_raster = plyr::round_any(obj@data$Y, resolution)

      by = list(Xr = x_raster,Yr = y_raster)

      selected = obj@data[, list(delete = .selectPulseToRemove(pulseID, n), t = gpstime), by=by]
      selected[, c("Xr", "Yr") := NULL]

      setorder(selected, t)
      setorder(obj@data, gpstime)

      selected = selected$delete
    }

    return(LAS(obj@data[selected]))
	}
)
