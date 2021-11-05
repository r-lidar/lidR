#' Voxelize a point cloud
#'
#' Reduce the number of points by voxelizing the point cloud. If the Intensity is part of the attributes
#' it is preserved and aggregated as \code{mean(Intensity)}. Other attributes cannot be aggregated and
#' are lost.
#'
#' @template param-las
#' @param res numeric. The resolution of the voxels. \code{res = 1} for a 1x1x1 cubic voxels. Optionally
#' \code{res = c(1,2)} for non-cubic voxels (1x1x2 cuboid voxel).
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' las2 = voxelize_points(las, 5)
#' #plot(las2, voxel = TRUE)
voxelize_points = function(las, res)
{
  assert_all_are_non_negative(res)

  UseMethod("voxelize_points", las)
}

#' @export
voxelize_points.LAS = function(las, res)
{
  Intensity <- NULL

  if (length(res) == 1L)
    res <- c(res,res)
  else if (length(res) > 2L)
    stop("Wrong resolution provided.")

  by <- group_grid_3d(las@data$X, las@data$Y, las@data$Z, res, c(0,0,0.5*res[2]))

  if ("Intensity" %in% names(las))
  {
    voxels <- las@data[, list(Intensity = as.integer(mean(Intensity))), by = by]
    data.table::setnames(voxels, c("X", "Y", "Z", "Intensity"))
  }
  else
  {
    data.table::setDT(by)
    data.table::setnames(by, c("X", "Y", "Z"))
    voxels <- unique(by)
  }

  output <- LAS(voxels, header = las@header, crs = st_crs(las), check = FALSE, index = las@index)
  return(output)
}

#' @export
voxelize_points.LAScluster = function(las, res)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)

  output <- voxelize_points(x, res)
  output <- clip_roi(output, st_bbox(las))
  return(output)
}

#' @export
voxelize_points.LAScatalog = function(las, res)
{
  opt_select(las) <- "xyzi"
  if (opt_wall_to_wall(las))
    opt_chunk_buffer(las) <- res[1]

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_apply(las, voxelize_points, res = res, .options = options)
  output  <- unlist(output)
  ctg     <- suppressMessages(suppressWarnings(readLAScatalog(output)))

  opt_copy(ctg) <- las
  return(ctg)
}
