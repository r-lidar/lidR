#' Compute metrics for a cloud of points
#'
#' Computes a series of descriptive statistics for a LiDAR dataset
#'
#' Computes a series of descriptive statistics for a LiDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set. See \link[lidR:gridMetrics]{gridMetrics}
#' to compute metrics on a grid. Basically there are no predifined metrics. Users
#' must write their own function to create metrics (see example). The following existing
#' function can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:canopyMatrix]{canopyMatrix}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:canopyClosure]{canopyClosure}}
#' \item{\link[lidR:fractal.dimension]{fractal.dimension}}
#' \item{\link[lidR:LAD]{LAD}}
#' }
#' @aliases cloudMetrics
#' @param obj An object of class \code{Lidar}
#' @param func The function to be applied to a cloud of points
#' @return It returns a \code{data.table} containing the metrics
#' @export cloudMetrics
#' @seealso \link[lidR:gridMetrics]{gridMetrics}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' cloudMetrics(lidar, max(Z))
#' cloudMetrics(lidar, mean(Z))
#'
#' # Define your own metric function
#' myMetrics = function(z, i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         hmean   = mean(z),
#'         hmax    = max(z),
#'         imean   = mean(i),
#'         angle   = mean(abs(angle))
#'         )
#'
#'    return(ret)
#'  }
#'
#' metrics = cloudMetrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID))
#' @importFrom magrittr %$%
setGeneric("cloudMetrics", function(obj, func){standardGeneric("cloudMetrics")})

#' @rdname cloudMetrics
setMethod("cloudMetrics", "Lidar",
	function(obj, func)
	{
	  func_call = substitute(func)
	  metric = obj@data %$% eval(func_call)
		return(metric)
	}
)