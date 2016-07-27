#' Clip LiDAR points within a rectangle
#'
#' Clip LiDAR points within a rectangle
#'
#' @param obj An object of class \code{Lidar}
#' @param xleft	a scalar of left x position.
#' @param ybottom	a scalar of bottom y position.
#' @param xright a scalar of right x position.
#' @param ytop a scalar of top y position.
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                  xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipRectangle
#' @importFrom data.table between
setGeneric("clipRectangle", function(obj, xleft, ybottom, xright, ytop, inside = TRUE){standardGeneric("clipRectangle")})

#' @rdname clipRectangle
setMethod("clipRectangle", "Lidar",
	function(obj, xleft, ybottom, xright, ytop, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, between(X, xleft, xright), between(Y, ybottom, ytop)))
	  else
	    return(extract(obj, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

	}
)

#' Clip LiDAR points within a polygon
#'
#' Clip LiDAR points within a polygon
#'
#' @aliases clipPolygon
#' @param obj An object of class \code{Lidar}
#' @param x	numerical array of x-coordinates of polygon
#' @param y	numerical array of y-coordinates of polygon
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipPolygon(x=c(685000, 685200, 685050),
#'                                y=c(5018000, 5018100, 5018200))
#'
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipPolygon
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})

#' @rdname clipPolygon
setMethod("clipPolygon", "Lidar",
	function(obj, x, y, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, pointsInPolygon(x,y,X,Y)))
	  else
	    return(extract(obj, !pointsInPolygon(x,y,X,Y)))
	}
)

#' Clip LiDAR points within a disc
#'
#' Clip LiDAR points within a disc
#'
#' @aliases clipCircle
#' @param obj An object of class \code{Lidar}
#' @param xcenter	a scalar. x disc center
#' @param ycenter	a scalar. y disc center
#' @param radius a scalar. Disc radius
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipCircle(685000, 5018000, 25)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipCircle
setGeneric("clipCircle", function(obj, xcenter, ycenter, radius, inside = TRUE){standardGeneric("clipCircle")})

#' @rdname clipCircle
setMethod("clipCircle", "Lidar",
	function(obj, xcenter, ycenter, radius, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
	  else
	    return(extract(obj, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
	}
)
