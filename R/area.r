#' Area of a LAS object
#'
#' Retrieve the area of a LAS object.
#'
#' area is computed with a convex hull. It is only an approximation if the shape of
#' the data is not convex.
#' @aliases area
#' @param obj An object of the class \code{LAS}
#' @return numeric. The area of the object computed with a convex hull in LAS coordinates units
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' area(lidar)
#' @seealso
#' \code{\link[lidR:convexHull]{convexHull} }
#' \code{\link[lidR:polygonArea]{polygonArea} }
#' @export area
setGeneric("area", function(obj){standardGeneric("area")})

#' @rdname area
setMethod("area", "LAS",
	function(obj)
	{
		hull = convexHull(obj@data$X, obj@data$Y)
		area = polygonArea(hull$x, hull$y)
		area = round(area,1)
		return(area)
	}
)
