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
#' @param .las An object of class \code{LAS}
#' @param func the function to be apply to each voxel.
#' @param res numeric. The size of the voxels
#' @param debug logical. If you encouter a non trivial error try \code{debug = TRUE}.
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table
#' has the class \code{lasmetrics3d} enabling easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Cloud of points is voxelized with a 1-meter resolution and in each voxel
#' # the number of points is computed.
#' grid_metrics3d(lidar, length(Z))
#'
#' # Cloud of points is voxelized with a 1-meter resolution and in each voxel
#' # the mean scan angle of points is computed.
#' grid_metrics3d(lidar, mean(ScanAngle))
#'
#' # Define your own metric function
#' myMetrics = function(i, angle, pulseID)
#' {
#'   ret = list(
#'      npulse  = length(unique(pulseID)),
#'      angle   = mean(angle),
#'      imean   = mean(i)
#'    )
#'
#'    return(ret)
#' }
#'
#' voxels = grid_metrics3d(lidar, myMetrics(Intensity, ScanAngle, pulseID))
#'
#' plot(voxels, "angle")
#' plot(voxels, "imean")
#' #etc.
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' @export
grid_metrics3d = function(.las, func, res = 1, debug = FALSE)
{
  stopifnotlas(.las)

  func_call = substitute(func)

  if(is(func_call, "name"))
    func_call = eval(func_call)

  if(debug)
    .las@data %$% eval(func_call) %>% .debug_grid_metrics(func_call)

  x_raster = round_any(.las@data$X, res)
  y_raster = round_any(.las@data$Y, res)
  z_raster = round_any(.las@data$Z-0.5*res, res)+0.5*res

  by = list(Xc = x_raster, Yc = y_raster, Zc = z_raster)

  stat <- .las@data[, c(eval(func_call)), by=by]

  n = names(stat)
  n[1:3] = c("X", "Y", "Z")
  setnames(stat, n)

  attr(stat, "class") = c("lasmetrics3d", attr(stat, "class"))
  attr(stat, "res") = res

  return(stat)
}
