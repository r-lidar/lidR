#' Plot voxelized LiDAR data
#'
#' This functions implements a 3D plot method for voxels objects
#'
#' By default the function plots points for fast display purposes. It can also plot real voxels.
#' @aliases plot.voxels
#' @param x An object of the class \code{voxels}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to color the points. Default is Z coordinates. Or a vector of colors.
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background. Default is black.
#' @param display character. By default is "points" to plot voxels as points for fast display. Could be set to "cubes" to display nice shaded cube. But "cubes" method is very very slow. It cannot be use for a large number of voxels.
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d} if display method is "points"
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' voxels = voxelize(lidar, 1, list(I = mean(Intensity)))
#' plot(voxels, color = "I", colorPalette = "heat.colors", trim=0.99)
#' @seealso
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[lidR:voxelize]{voxelize}
#' @export
#' @importFrom rgl points3d open3d rgl.bg
#' @importFrom grDevices heat.colors terrain.colors topo.colors
plot.voxels = function(x, y, color = "Z", colorPalette = "height.colors", bg = "black", display = "points", ...)
{
  inargs <- list(...)

  trim = ifelse(is.null(inargs$trim), 1, inargs$trim)

  inargs$col = color

  if(length(color) == 1)
  {
    if(color %in% names(x))
    {
      data = unlist(x[,color, with = FALSE])

      if(is.numeric(data))
      {
        inargs$col = .colorPalette(data-min(data), trim, colorPalette)
        inargs$col[is.na(inargs$col)] = "lightgray"
      }
      else if(is.character(data))
        inargs$col = data
    }
  }

  if(display == "points")
  {
    rgl::open3d()
    rgl::rgl.bg(color = bg)
    do.call(rgl::points3d, c(list(x=x$X, y=x$Y, z=x$Z), inargs))
  }
  else if(display == "cubes")
  {
    rgl::open3d()
    rgl::rgl.bg(color = bg)
    x %$% cube(x$X, x$Y, x$Z, inargs$col, scale = attr(x, "res"))
  }
  else
    lidRError("VOX1")
}

cube <- function(x, y, z, col, scale = 1)
{

  mycube <- rgl::cube3d()

  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2

  for (i in 1:length(x))
  {
    # Add cube border
    bcube <- mycube
    bcube$material$lwd <- 2
    bcube$material$front <- 'line'
    bcube$material$back <- 'line'
    bcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d

    # Add cube fill
    fcube <- mycube
    fcube$vb[4,] <- fcube$vb[4,]*1.01
    fcube$material$col <- col[i]
    fcube %>% rgl::translate3d(x[i], y[i], z[i]) %>% shade3d
  }
}