#' Area of a Lidar object
#'
#' Retrieve the area of a Lidar object.
#'
#' area is computed with a convex hull. It is only an approximation if the shape of
#' the data is not convex.
#' @aliases area
#' @param obj An object of the class \code{Lidar}
#' @return numeric. The area of the object computed with a convex hull in Lidar coordinates units
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' area(lidar)
#' @seealso
#' \code{\link[lidR:convexHull]{convexHull} }
#' \code{\link[lidR:polygonArea]{polygonArea} }
#' @export area
setGeneric("area", function(obj){standardGeneric("area")})

#' @rdname area
setMethod("area", "Lidar",
	function(obj)
	{
		hull = convexHull(obj@data$X, obj@data$Y)
		area = polygonArea(hull$x, hull$y)
		area = round(area,1)
		return(area)
	}
)