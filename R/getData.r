#' Get LiDAR data
#'
#' Return the slot @data from a \code{Lidar} object
#'
#' @aliases getData
#' @param obj An object of class \code{Lidar}
#' @return It returns a \code{data.table} containing the LiDAR data
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' getData(lidar)
#' @export getData
setGeneric("getData", function(obj){standardGeneric("getData")})

#' @rdname getData
setMethod("getData", "Lidar",
	function(obj)
	{
		return(obj@data)
	}
)