#' Plot a Catalog object
#'
#' This functions implements a \link[graphics:plot]{plot} method for a Catalog objects
#'
#' @param x A Catalog object
#' @param y Unused (inherited from base plot)
#' @param \dots Unused (inherited from base plot)
#' @export
plot.Catalog = function(x, y, ...)
{
  headers = x@headers

  xmin = min(headers$Min.X)
  xmax = max(headers$Max.X)
  ymin = min(headers$Min.Y)
  ymax = max(headers$Max.Y)

  plot(0,0, xlim=c(xmin, xmax), ylim = c(ymin, ymax), col="white", asp=1, xlab="X", ylab="Y")
  rect(headers$Min.X, headers$Min.Y, headers$Max.X, headers$Max.Y)
}