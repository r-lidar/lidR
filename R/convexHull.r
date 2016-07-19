#' Compute the convex hull of set of points
#'
#' Computes the convex hull of the set of points specified.
#'
#' @param x coordinates vectors of points
#' @param y coordinates vectors of points
#' @return A data frame with the coordinates of the unique points lying on the convex hull, in clockwise order. The first point is repeated to close the hull.
#' @examples
#' x = runif(20)
#' y = runif(20)
#' hull = convexHull(x,y)
#'
#' plot(x,y)
#' lines(hull)
#' @seealso \link[grDevices:chull]{chull}
#' @export convexHull
#' @importFrom grDevices chull
convexHull = function(x, y)
{
	data = data.frame(x,y)
	ch <- grDevices::chull(x,y)
	coords <- data[c(ch, ch[1]), ]
	rownames(coords) = NULL
	return(coords)
}
