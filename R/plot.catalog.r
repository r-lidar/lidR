#' Plot a Catalog object
#'
#' This functions implements a \link[graphics:plot]{plot} method for a Catalog objects
#'
#' @param x A Catalog object
#' @param y Unused (inherited from base plot)
#' @param \dots Unused (inherited from base plot)
#' @export
#' @examples
#' \dontrun{
#'
#' catalog = Catalog("<Path to a folder containing a set of .las files>")
#' plot(catalog)
#' }
#' @importFrom graphics plot rect text
#' @importFrom magrittr %$%
plot.Catalog = function(x, y, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- filename <- NULL

  headers = x@headers

  xmin = min(headers$Min.X)
  xmax = max(headers$Max.X)
  ymin = min(headers$Min.Y)
  ymax = max(headers$Max.Y)

  xcenter = (headers$Min.X+headers$Max.X)/2
  ycenter = (headers$Min.Y+headers$Max.Y)/2

  graphics::plot(xcenter, ycenter, xlim=c(xmin, xmax), col="white", ylim = c(ymin, ymax), asp=1, xlab="X", ylab="Y")
  headers %$% graphics::rect(Min.X, Min.Y, Max.X, Max.Y)
}

