#' Calculates the area of a polygon
#'
#' @param x numeric. x coordinates of the polygon
#' @param y numeric. y coordinates of the polygon
#' @export polygonArea
polygonArea = function(x, y)
{
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!is.numeric(x) || !is.numeric(y) ) stop("Arguments 'x' and 'y' must be real")
  if (length(x) != length(y)) stop("Argument 'x' and 'y' must be of same size")

	area = 0;
	j = length(x)

	for (i in 1:j)
	{
		area = area + (x[j]+x[i])*(y[j]-y[i]);
		j = i;
	}

	area  = abs(area*0.5)

	return(area)
}