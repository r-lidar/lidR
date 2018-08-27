#' Filter the surface points
#'
#' This routine creates a grid with a given resolution and filters the point cloud by selecting the
#' highest point within each cell. This function is different from \link{grid_canopy} even if the overall
#' concept is exactly the same. \code{grid_canopy} attributes to each cell of a raster the elevation
#' of the highest points and the point cloud is rasterized. Here, there is no rasterization, the function
#' takes a point cloud as input and returns a point cloud as output. Coordinates and attributes are preserved.
#'
#' @template section-supported-option-lasfilter
#'
#' @template LAScatalog
#'
#' @template param-las
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#' @family lasfilters
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' subset = lasfiltersurfacepoints(las, 2)
#' plot(subset)
lasfiltersurfacepoints = function(las, res)
{
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)

  UseMethod("lasfiltersurfacepoints", las)
}

#' @export
lasfiltersurfacepoints.LAS = function(las, res)
{
  Z   <- NULL
  by  <- group_grid(las@data$X, las@data$Y, res)
  sub <- las@data[las@data[, .I[which.max(Z)], by = by]$V1]
  las <- LAS(sub, las@header, las@proj4string)
  return(las)
}

#' @export
lasfiltersurfacepoints.LAScluster = function(las, res)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- lasfiltersurfacepoints(x, res)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasfiltersurfacepoints.LAScatalog = function(las, res)
{
  set_buffer(las) <- 0.1*res
  set_select(las) <- "*"

  output      <- catalog_apply2(las, lasfiltersurfacepoints, res = res, need_buffer = FALSE, check_alignement = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}