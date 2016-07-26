#' Plot a Catalog object
#'
#' This functions implements a \link[graphics:plot]{plot} method for a Catalog objects
#'
#' @param x A Catalog object
#' @param y Unused (inherited from base plot)
#' @param \dots Unused (inherited from base plot)
#' @export
#' @importFrom graphics plot rect text
#' @importFrom magrittr %$%
plot.Catalog = function(x, y, ...)
{
  headers = x@headers

  xmin = min(headers$Min.X)
  xmax = max(headers$Max.X)
  ymin = min(headers$Min.Y)
  ymax = max(headers$Max.Y)

  graphics::plot(0,0, xlim=c(xmin, xmax), ylim = c(ymin, ymax), col="white", asp=1, xlab="X", ylab="Y")
  headers %$% graphics::rect(Min.X, Min.Y, Max.X, Max.Y)
  headers %$% graphics::text((Min.X + Max.X)/2, (Min.Y + Max.Y)/2, basename(filename))
}