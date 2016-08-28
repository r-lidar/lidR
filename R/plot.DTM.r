#' Plot a wireframe of a DTM object
#'
#' @param x An object of the class \code{DTM}
#' @param y Unused (inherited from R base)
#' @param add logical. if TRUE, add to current 3D plot.
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl:surface3d]{surface3d}
#' @export
#' @importFrom rgl surface3d open3d rgl.bg
plot.DTM = function(x, y, add = FALSE, bg = "black", ...)
{
  inargs <- list(...)

  if(!add) rgl::open3d()

  rgl::rgl.bg(color = bg)

  rgl::surface3d(x$x, x$y, x$z, front="lines", col="white", ...)
}