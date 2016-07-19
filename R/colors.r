#' @importFrom magrittr %>%
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