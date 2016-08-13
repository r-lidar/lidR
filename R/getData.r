#' Get LiDAR data
#'
#' Return the slot @data from a \code{LAS} object
#'
#' @aliases getData
#' @param obj An object of class \code{LAS}
#' @return It returns a \code{data.table} containing the LiDAR data
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' getData(lidar)
#' @export getData
setGeneric("getData", function(obj){standardGeneric("getData")})

#' @rdname getData
setMethod("getData", "LAS",
	function(obj)
	{
		return(obj@data)
	}
)
