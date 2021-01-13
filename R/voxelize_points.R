# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2018 Jean-Romain Roussel
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
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' las2 = voxelize_points(las, 2)
#' #plot(las2)
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

  if ("Intensity" %in% names(las@data))
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

  output <- LAS(voxels, header = las@header, proj4string = las@proj4string, check = FALSE, index = las@index)
  return(output)
}

#' @export
voxelize_points.LAScluster = function(las, res)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)

  output <- voxelize_points(x, res)
  output <- clip_roi(output, raster::extent(las))
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
