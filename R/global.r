#' @importFrom magrittr %>% %<>% %$% divide_by
#' @importFrom data.table := setnames is.data.table as.data.table setorder data.table between rbindlist
#' @import methods

#' @importFrom dplyr lag
.IdentifyPulse = function(return.number)
{
  boo = dplyr::lag(return.number) >= return.number
  boo[1] = TRUE
  return(cumsum(boo))
}

#' @importFrom dplyr lag
.IdentifyFlightlines = function(time, t = 30)
{
  boo = time - dplyr::lag(time) > t
  boo[1] = TRUE
  return(cumsum(boo))
}

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

.colorPalette = function(x, q = 1, palette = "terrain.colors")
{
  fun <- paste(palette, "(n+1)", sep="")
  n   <- x %>% quantile(q, na.rm=T) %>% round

	colors_map <- eval(parse(text=fun))
	colors     <- colors_map[round(x, 0)+1]

	return(colors)
}

#' height.colors
#'
#' Create a vector of n contiguous colors of elevations.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @importFrom grDevices colorRampPalette
#' @export height.colors
height.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("blue", "cyan2", "green3", "yellow", "red"))
  return(colfunc(n))
}

#' Color palette of green
#'
#' Create a vector of n contiguous colors of green.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[grDevices:colorRamp]{colorRampPalette}
#' @importFrom grDevices colorRampPalette
#' @export forest.colors
forest.colors = function(n)
{
  colfunc <- grDevices::colorRampPalette(c("darkgreen", "lightgreen"))
  return(colfunc(n))
}