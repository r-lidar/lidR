#' Pulse density surface model
#'
#' Creates a pulse density map using a LiDAR cloud of points.
#'
#' @aliases pulseDensity
#' @param obj An object of class \code{LAS}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 units i.e. 16 square units cells.
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which which enables easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' lidar %>% pulseDensity(5) %>% plot
#' lidar %>% pulseDensity(10) %>% plot
#' @seealso
#' \code{\link[lidR:gridMetrics]{gridMetrics}}
#' @export pulseDensity
#' @importFrom dplyr rename
setGeneric("pulseDensity", function(obj, res = 4){standardGeneric("pulseDensity")})

#' @rdname pulseDensity
setMethod("pulseDensity", "LAS",
	function(obj, res = 4)
	{
	  pulseID <- V1 <- NULL

		ret = gridMetrics(obj, res, length(unique(pulseID))/res^2) %>% dplyr::rename(Z = V1)
    return(ret)
	}
)
