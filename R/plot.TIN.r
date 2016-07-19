#' Plot triangular irregular network TIN
#'
#' This functions implements a \link[graphics:plot]{plot} method for class TIN
#'
#' @param x An object of class \code{TIN}
#' @param ... Unused
#' @examples
#' x = runif(20)
#' y = runif(20)
#' z = runif(20)
#' tin = TIN(x, y, z)
#' plot(tin)
#' @seealso
#' \link[lidR:TIN]{TIN}
#' @rdname plot.TIN
#' @export
#' @method plot TIN
plot.TIN = function(x, ...)
{
  minx = min(x$x1, x$x2, x$x3)
  maxx = max(x$x1, x$x2, x$x3)
  miny = min(x$y1, x$y2, x$y3)
  maxy = max(x$y1, x$y2, x$y3)

  plot(minx,miny,xlim=c(minx,maxx), ylim=c(miny,maxy), col="white", asp=1, xlab="X", ylab="Y")

  invisible( apply(x, 1, function(x) { lines(c(x[1], x[2], x[3], x[1]), c(x[4], x[5], x[6], x[4])) } ))
}