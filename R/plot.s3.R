#' Plot voxelized LiDAR data
#'
#' This function implements a 3D plot method for 'lasmetrics3d' objects
#'
#' @param x An object of the class `lasmetrics3d`
#' @param y Unused (inherited from R base)
#' @param \dots Supplementary parameters for \link[lidR:plot]{plot}. The function internally uses the
#' same plot function than LAS objects.
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' voxels = voxel_metrics(lidar, list(Imean = mean(Intensity)), res = 5)
#' plot(voxels, color = "Imean", colorPalette = heat.colors(50), trim=60)
#' }
#' @export
#' @method plot lasmetrics3d
#' @md
plot.lasmetrics3d = function(x, y, ...)
{
  cl <- class(x)
  on.exit(data.table::setattr(x, "class", cl))
  header = rlas::header_create(x)
  las = LAS(x, header, check = FALSE)
  attr(las, "res") <- attr(x, "res")
  plot(las, ...)
}

#' Add a spatial object to a point cloud scene
#'
#' Add a raster (`raster`, `stars` `terra`) object that represents a digital terrain model or a
#' `SpatialPointsDataFrame` or `sf` that represents tree tops to a point cloud scene. To add elements
#' to a scene with a point cloud plotted with the function plot from lidR, the functions `add_*`
#' take as first argument the output of the plot function (see examples), because the plot function
#' does not plot the actual coordinates of the point cloud, but offset values. See function
#' \link[=plot]{plot} and its argument `clear_artifacts` for more details. It works only
#' with `rgl` i.e. `backend = "rgl"` which is the default.
#'
#' @param dtm An object of the class `RasterLayer` or `stars` or `SpatRaster`
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl]{surface3d} or
#' \link[rgl:spheres]{spheres3d}.
#' @param x The output of the function plot used with a LAS object.
#' @param ttops A `SpatialPointsDataFrame` or `sf/sfc` that contains tree tops coordinates.
#' @param flightlines A `SpatialPointsDataFrame` or `sf` that contains flightlines coordinates.
#' @param z character. The name of the attribute that contains the height of the tree tops or of the
#' flightlines. Only for XY geometries Ignored if the input have XYZ geometries
#' @param clear_artifacts logical. It is a known and documented issue that 3D visualisation with
#' \code{rgl} displays artifacts. The points and lines are inaccurately positioned in the space and thus
#' the rendering may look false or weird. This is because `rgl` computes with single precision `float`.
#' To fix this, the objects are shifted to (0,0) to reduce the number of digits needed to represent
#' their coordinates. The drawback is that the objects are not plotted at their actual coordinates.
#'
#' @name plot_3d
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' dtm <- rasterize_terrain(las, algorithm = tin())
#' ttops <- locate_trees(las, lmf(ws = 5))
#'
#' plot_dtm3d(dtm)
#'
#' x <- plot(las)
#' add_dtm3d(x, dtm)
#' add_treetops3d(x, ttops)
#'
#' plot(las) |> add_dtm3d(dtm) |> add_treetops3d(ttops)
#' }
NULL

#' @rdname plot_3d
#' @export
#' @md
plot_dtm3d = function(dtm, bg = "black", clear_artifacts = TRUE, ...)
{
  rgl::open3d()
  rgl::rgl.bg(color = bg)
  shift = c(0,0)

  if (clear_artifacts)
  {
    bbox  <- raster_bbox(dtm)
    shift <- c(bbox$xmin, bbox$ymin)
  }

  add_dtm3d(shift, dtm, ...)

  .pan3d(2)

  if (clear_artifacts)
    return(invisible(shift))
  else
    return(invisible(c(0,0)))
}

#' @rdname plot_3d
#' @export
add_dtm3d = function(x, dtm, ...)
{
  args <- list(...)

  assert_is_numeric(x)
  assert_is_of_length(x, 2)

  if (!is_raster(dtm))
    stop("'dtm' is not a raster")

  res <- raster_as_matrix(dtm, downsample = TRUE)

  if (is.null(args$front))
    args$front <- "lines"

  if (is.null(args$col))
    args$col <- "white"

  args$x <- res$x - x[1]
  args$y <- res$y - x[2]
  args$z <- res$z

  do.call(rgl::surface3d, args)
  return(invisible(x))
}

#' @rdname plot_3d
#' @export
add_treetops3d = function(x, ttops, z = "Z", ...)
{
  args <- list(...)

  assert_is_numeric(x)
  assert_is_of_length(x, 2)

  if (is(ttops, "SpatialPointsDataFrame"))
    ttops <- sf::st_as_sf(ttops)

  if (!is(ttops, "sf"))
    stop("'ttops' is not a SpatialPointsDataFrame or sf")

  if (is.null(args$size))
    args$size <- 5

  if (is.null(args$col))
    args$col <- "red"

  args$add <- TRUE
  coords   <- sf::st_coordinates(ttops)
  args$x   <- coords[,1] - x[1]
  args$y   <- coords[,2] - x[2]

  if (ncol(coords) == 3)
    args$z <- coords[,3]
  else
    args$z <- ttops[[z]]

  do.call(rgl::spheres3d, args)
  return(invisible(x))
}

#' @rdname plot_3d
#' @export
add_flightlines3d = function(x, flightlines, z = "Z", ...)
{
  return(add_treetops3d(x,flightlines, z = "Z", ...))
}


