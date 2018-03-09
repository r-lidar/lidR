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
#'
#' @return A LAS object
#' @export
#' @family lasfilters
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = lasfiltersurfacepoints(las, 2)
#' plot(subset)
lasfiltersurfacepoints = function(las, res)
{
  f = function(z, id) {
    i = which.max(z)
    return(list(id = id[i]))
  }

  npoints = nrow(las@data)
  las@data[, pointID := 1:npoints]
  by = group_grid(las@data$X, las@data$Y, res)
  keep = las@data[, f(Z, pointID), by = by][, Xgrid := NULL][, Ygrid := NULL][]
  cloud = las@data[keep$id]
  las@data[, pointID := NULL]

  return(LAS(cloud, las@header))
}