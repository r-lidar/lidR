#' Canopy surface model
#'
#' Creates a canopy surface model using a LiDAR cloud of points.
#'
#' The algorithm used is the local maximum algorithm. It assigns the
#' elevation of the highest return within each grid cell to the grid cell center.
#' @aliases  canopyModel
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 2 units i.e. 4 square units cells.
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) see \link[lidR:gridMetrics]{gridMetrics}
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which enables easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' # Local maximum algorithm with a resolution of 2 meters
#' lidar %>% canopyModel(2) %>% plot
#' lidar %>% canopyModel(2) %>% plot3d
#'
#' # Local maximum and TIN algorithm on a plot with a resolution of 0.5 meters
#' forestplot = clipCircle(lidar, 685000, 5017900, 25)
#' forestplot %>% canopyModel(.5) %>% plot
#' @seealso
#' \code{\link[lidR:gridMetrics]{gridMetrics}}
#' \code{\link[lidR:clipCircle]{clipCircle}}
#' @export canopyModel
#' @importFrom dplyr rename
#' @importFrom magrittr %>% %$%
setGeneric("canopyModel", function(obj, res = 2, start = c(0,0)){standardGeneric("canopyModel")})

#' @rdname canopyModel
setMethod("canopyModel", "Lidar",
	function(obj, res = 2, start=c(0,0))
	{
	  X <- Y <- Z <- V1 <- NULL

	  ret = gridMetrics(obj, res, max(Z), start) %>% dplyr::rename(Z = V1)

	  #lidRError("LDR6")

    return(ret)
	}
)