#' Filter the surface points
#'
#' This routine creates a grid with a given resolution and filters the point cloud by selecting the
#' highest elevation point within each cell.
#'
#' This function is different from \link{grid_canopy} even if the overall concept is exactly the same.
#' \code{grid_canopy} attributes to each cell the elevation of the highest points and he point cloud is
#' rasterized. Here, there is no rasterization, the function have a point cloud as input and a point
#' cloud as output. Coordinates and attributes are preserved.
#'
#' @param las A LAS object
#' @param res numeric. The resolution of the grid used to filter the point cloud
#' @param smooth numeric. Smooth the output point cloud using a moving average windows at the point
#' level. This is the size of the moving windows. Default = 0 i.e. no smothing.
#' @return A LAS object
#' @export
#' @family lasfilters
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = lasfiltersurfacepoints(las, 2)
#' plot(subset)
lasfiltersurfacepoints = function(las, res, smooth = 0)
{
  Z <- NULL
  by  = group_grid(las@data$X, las@data$Y, res)
  sub = las@data[las@data[, .I[which.max(Z)], by = by]$V1]

  las = LAS(sub, las@header)
  Zsmoothed = C_lassmooth(las, smooth)
  las@data[, Z := Zsmoothed]
  return(las)
}