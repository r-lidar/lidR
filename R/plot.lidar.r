#' Plot LiDAR data
#'
#' This functions implements a 3D plot method for Lidar objects
#'
#' @aliases plot plot.Lidar
#' @param x An object of the class \code{Lidar}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to color the points. Default is Z coordinates. Or a vector of colors.
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' plot(lidar)
#' plot(lidar, color = "Intensity", colorPalette = "heat.colors")
#' @seealso
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[lidR:Lidar]{Class Lidar}
#' @export
#' @importFrom rgl points3d open3d rgl.bg
#' @importFrom grDevices heat.colors terrain.colors topo.colors
plot.Lidar = function(x, y, color = "Z", colorPalette = "height.colors", bg = "black",  ...)
{
  inargs <- list(...)

  trim = ifelse(is.null(inargs$trim), 1, inargs$trim)

  inargs$col = color

  if(length(color) == 1)
  {
    if(color %in% names(x@data))
    {
      data = unlist(x@data[,color, with = FALSE])

      if(is.numeric(data))
      {
        inargs$col = .colorPalette(data-min(data), trim, colorPalette)
        inargs$col[is.na(inargs$col)] = "lightgray"
      }
      else if(is.character(data))
        inargs$col = data
    }
  }

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, c(list(x=x@data$X, y=x@data$Y, z=x@data$Z), inargs))
}