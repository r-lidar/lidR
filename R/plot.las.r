#' Plot LiDAR data
#'
#' This functions implements a 3D plot method for LAS objects
#'
#' @aliases plot plot.LAS
#' @param x An object of the class \code{LAS}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to color the points. Default is Z coordinates. Or a vector of colors.
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background. Default is black.
#' @param trim numeric. Enables trimming of values when outliers break the color palette range.
#' Default is 1 meaning that the whole range of the values is used for the color palette.
#' 0.9 means thant 10% of the hightest values are not used to defined the colors palette.
#' In this case the values higher that the 90th percentile are set to the highest color. They are not removed.
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' plot(lidar)
#'
#' # Outliers of intensity breaks the color range. Use the trim parameter.
#' plot(lidar, color = "Intensity", colorPalette = heat.colors)
#' plot(lidar, color = "Intensity", colorPalette = heat.colors, trim = 0.99)
#' @seealso
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' \link[lidR:LAS]{Class LAS}
#' @export
#' @importFrom rgl points3d open3d rgl.bg
#' @importFrom grDevices heat.colors terrain.colors topo.colors
plot.LAS = function(x, y, color = "Z", colorPalette = height.colors, bg = "black",  trim = 1, ...)
{
  inargs <- list(...)

  inargs$col = color

  if(length(color) == 1)
  {
    if(color %in% names(x@data))
    {
      data = unlist(x@data[,color, with = FALSE])

      if(is.numeric(data))
        inargs$col = set.colors(data, colorPalette, 50, trim)
      else if(is.character(data))
        inargs$col = data
      else if(is.logical(data))
        inargs$col = set.colors(as.numeric(data), colorPalette, 2)

      inargs$col[is.na(inargs$col)] = "lightgray"
    }
  }

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, c(list(x=x@data$X, y=x@data$Y, z=x@data$Z), inargs))
}
