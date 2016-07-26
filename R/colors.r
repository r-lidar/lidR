#' Automatic colorization
#'
#' Attribute a color to each element of a vector
#'
#' @param x A vector
#' @param palette function. A color palette function. Default is \code{height.colors} provided by the package lidR
#' @param ncolors numeric. The number of colors in the palette.
#' @param trim numeric.
#' @importFrom magrittr %>%
#' @importFrom stats quantile
set.colors = function(x, palette, ncolors = 50, trim = 1)
{

  if(trim < 1)
  {
    n = x %>% quantile(trim)
    x[x > n] = n
  }

  colors = palette(ncolors)[as.numeric(cut(x, breaks = ncolors))]

	return(colors)
}

#' height.colors
#'
#' Create a vector of n contiguous colors of elevations.
#'
#' @param n The number of colors (> 1) to be in the palette
#' @seealso
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
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
#' Create a vector of n contiguous colors of green from darkgreen to lightgreen.
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