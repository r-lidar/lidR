round_any <- function(x, accuracy, f = round)
{
  f(x / accuracy) * accuracy
}

convex_hull = function(x, y)
{
	data = data.frame(x,y)
	ch <- grDevices::chull(x,y)
	coords <- data[c(ch, ch[1]), ]
	rownames(coords) = NULL
	return(coords)
}

polygon_area = function(x, y)
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

area = function(x, y)
{
  hull = convex_hull(x, y)
  area = polygon_area(hull$x, hull$y)
  area = round(area,1)
  return(area)
}