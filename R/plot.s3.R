# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================

#' Plot voxelized LiDAR data
#'
#' This function implements a 3D plot method for 'lasmetrics3d' objects
#'
#' @param x An object of the class \code{'lasmetrics3d'}
#' @param y Unused (inherited from R base)
#' @param \dots Supplementary parameters for \link[lidR:plot]{plot}. The function internally uses the
#' same plot function than LAS objects.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' voxels = voxel_metrics(lidar, list(Imean = mean(Intensity)), res = 5)
#' plot(voxels, color = "Imean", colorPalette = heat.colors(50), trim=60)
#' @export
#' @method plot lasmetrics3d
plot.lasmetrics3d = function(x, y, ...)
{
  cl <- class(x)
  on.exit(data.table::setattr(x, "class", cl))
  header = rlas::header_create(x)
  las = LAS(x, header, check = FALSE)
  plot(las, ...)
}

#' Add a spatial object to a point cloud scene
#'
#' Add a \code{RasterLayer} object that represents a digital terrain model or a
#' \code{SpatialPointsDataFrame} that represents tree tops to a point cloud scene. To add elements
#' to a scene with a point cloud plotted with the function plot from lidR, the functions \code{add_*}
#' take as first argument the output of the plot function (see examples), because the plot function
#' does not plot the actual coordinates of the point cloud, but offsetted values. See function
#' \link[=plot]{plot} and its argument \code{clear_artifacts} for more details. It works only
#' with \code{rgl} i.e. \code{backend = "rgl"} which is the default.
#'
#' @param dtm An object of the class \code{RasterLayer}
#' @param bg The color for the background. Default is black.
#' @param \dots Supplementary parameters for \link[rgl]{surface3d} or
#' \link[rgl:spheres]{spheres3d}.
#' @param x The output of the function plot used with a LAS object.
#' @param ttops A SpatialPointsDataFrame that contains tree tops coordinates.
#' @param flightlines A SpatialPointsDataFrame that contains flightlines coordinates.
#' @param z character. The name of the attribute that contains the height of the tree tops or of the flightlines.
#' @param clear_artifacts logical. It is a known and documented issue that 3D visualisation with
#' \code{rgl} displays artifacts. The points and lines are inaccurately positioned in the space and thus
#' the rendering may look false or weird. This is because \code{rgl} computes with single precision \code{float}.
#' To fix this, the objects are shifted to (0,0) to reduce the number of digits needed to represent
#' their coordinates. The drawback is that the objects are not plotted at their actual coordinates.
#'
#' @name plot_3d
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-keep_xy 273450 273600 5274450 5274600")
#'
#' dtm = grid_terrain(las, algorithm = tin())
#' ttops <- find_trees(las, lmf(ws = 5))
#'
#' plot_dtm3d(dtm)
#'
#' x = plot(las)
#' add_dtm3d(x, dtm)
#' add_treetops3d(x, ttops)
#'
#' \dontrun{
#' library(magrittr)
#' plot(las) %>% add_dtm3d(dtm) %>% add_treetops3d(ttops)
#' }
NULL

#' @rdname plot_3d
#' @export
plot_dtm3d = function(dtm, bg = "black", clear_artifacts = TRUE, ...)
{
  rgl::open3d()
  rgl::rgl.bg(color = bg)
  shift = c(0,0)

  if (clear_artifacts)
  {
    bbox  <- raster::extent(dtm)
    shift <- c(bbox@xmin, bbox@ymin)
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

  if (!is(dtm, "RasterLayer"))
    stop("'dtm' is not RasterLayer.")

  if (is.null(args$front))
    args$front <- "lines"

  if (is.null(args$col))
    args$col <- "white"

  mx <-  t(apply(raster::as.matrix(dtm), 2, rev))
  x_ <- sort(raster::xFromCol(dtm, 1:raster::ncol(dtm))) - x[1]
  y_ <- sort(raster::yFromRow(dtm, 1:raster::nrow(dtm))) - x[2]

  args$x <- x_
  args$y <- y_
  args$z <- mx

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

  if (!is(ttops, "SpatialPointsDataFrame"))
    stop("'ttops' is not a SpatialPointsDataFrame")

  if (is.null(args$size))
    args$size <- 5

  if (is.null(args$col))
    args$col <- "red"

  args$add <- TRUE
  args$x   <- ttops@coords[,1] - x[1]
  args$y   <- ttops@coords[,2] - x[2]
  args$z   <- ttops@data[[z]]

  do.call(rgl::spheres3d, args)
  return(invisible(x))
}

#' @rdname plot_3d
#' @export
add_flightlines3d = function(x, flightlines, z = "Z", ...)
{
  return(add_treetops3d(x,flightlines, z = "Z", ...))
}


