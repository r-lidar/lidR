#' Extent
#'
#' Returns an Extent object of a \code{Lidar} object.
#'
#' @aliases extent
#' @param x An object of the class \code{Lidar}
#' @param \dots Unused
#' @return Extent object
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' extent(lidar)
#' @seealso \code{\link[raster:extent]{raster::extent} }
#' @export extent
#' @importFrom raster extent
setGeneric("extent", function(x){standardGeneric("extent")})

#' @rdname extent
setMethod("extent", "Lidar",
	function(x)
	{
		return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
	}
)