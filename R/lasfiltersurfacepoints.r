#' Filter the surface points
#'
#' This routine creates a grid with a given resolution and filters the point cloud by selecting the
#' highest point within each cell.
#'
#' This function is different from \link{grid_canopy} even if the overall concept is exactly the same.
#' \code{grid_canopy} attributes to each cell the elevation of the highest points and the point cloud is
#' rasterized. Here, there is no rasterization, the function takes a point cloud as input and a point
#' cloud as output. Coordinates and attributes are preserved.
#'
#' @param las A LAS object
#' @param res numeric. The resolution of the grid used to filter the point cloud
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
  stopifnotlas(las)
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)

  Z <- NULL
  by  = group_grid(las@data$X, las@data$Y, res)
  sub = las@data[las@data[, .I[which.max(Z)], by = by]$V1]
  las = LAS(sub, las@header, las@crs)
  return(las)
}