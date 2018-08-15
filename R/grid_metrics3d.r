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



#' Voxelize the space and compute metrics for each voxel
#'
#' Voxelize the cloud of points and compute a series of descriptive statistics for
#' each voxel.
#'
#' Voxelize creates a 3D matrix of voxels with a given resolution. It creates a voxel
#' from the cloud of points if there is at least one point in the voxel. For each voxel
#' the function allows computation of one or several derived metrics in the same way as
#' the \link[lidR:grid_metrics]{grid_metrics} functions.
#' Basically there are no predefined metrics. Users must write their own function to create metrics.
#' Voxelize will dispatch the LiDAR data for each voxel in the user's function. The user writes their
#' function without considering voxels, only a cloud of points (see example).
#'
#' @param las An object of class \code{LAS}
#' @param func the function to be apply to each voxel.
#' @param res numeric. The size of the voxels
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table
#' has the class \code{lasmetrics3d} enabling easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Cloud of points is voxelized with a 3-meter resolution and in each voxel
#' # the number of points is computed.
#' grid_metrics3d(lidar, length(Z), 3)
#'
#' # Cloud of points is voxelized with a 3-meter resolution and in each voxel
#' # the mean scan angle of points is computed.
#' grid_metrics3d(lidar, mean(ScanAngle), 3)
#'
#' \dontrun{
#' # Define your own metric function
#' myMetrics = function(i, angle)
#' {
#'   ret = list(
#'      npoints = length(i),
#'      angle   = mean(angle),
#'      imean   = mean(i)
#'    )
#'
#'    return(ret)
#' }
#'
#' voxels = grid_metrics3d(lidar, myMetrics(Intensity, ScanAngle), 3)
#'
#' plot(voxels, color = "angle")
#' plot(voxels, color = "imean")
#' #etc.
#' }
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' @export
grid_metrics3d = function(las, func, res = 1)
{
  stopifnotlas(las)
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_non_negative(res)

  call <- substitute(func)
  by   <- group_grid_3d(las@data$X, las@data$Y, las@data$Z, res, c(0,0,0))
  stat <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = by]
  data.table::setnames(stat, c("Xgrid", "Ygrid", "Zgrid"), c("X", "Y", "Z"))
  data.table::setattr(stat, "class", c("lasmetrics3d", attr(stat, "class")))
  data.table::setattr(stat, "res", res)
  return(stat)
}
