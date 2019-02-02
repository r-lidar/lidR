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
#' This is a 3D version of \link{grid_metrics}. It creates a 3D matrix of voxels with a given resolution.
#' It creates a voxel from the cloud of points if there is at least one point in the voxel. For each voxel
#' the function allows computation of one or several derived metrics in the same way as the \link{grid_metrics}
#' functions. The function will dispatch the LiDAR data for each voxel in the user's function (see \link{grid_metrics}).
#'
#' @param las An object of class \code{LAS}.
#'
#' @param func expression. The function to be applied to each voxel (see also \link{grid_metrics}).
#'
#' @param res numeric. The resolution of the voxels. \code{res = 1} for a 1x1x1 cubic voxels. Optionally
#' \code{res = c(1,2)} for non-cubic voxels (1x1x2 cuboid voxel).
#'
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table
#' has the class \code{lasmetrics3d} enabling easier plotting.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # Cloud of points is voxelized with a 3-meter resolution and in each voxel
#' # the number of points is computed.
#' grid_metrics3d(las, length(Z), 3)
#'
#' # Cloud of points is voxelized with a 3-meter resolution and in each voxel
#' # the mean scan angle of points is computed.
#' grid_metrics3d(las, mean(Intensity), 3)
#'
#' \dontrun{
#' # Define your own metric function
#' myMetrics = function(i)
#' {
#'   ret = list(
#'      npoints = length(i),
#'      imean   = mean(i)
#'    )
#'
#'    return(ret)
#' }
#'
#' voxels = grid_metrics3d(las, myMetrics(Intensity), 3)
#'
#' plot(voxels, color = "imean")
#' #etc.
#' }
grid_metrics3d = function(las, func, res = 1)
{
  stopifnotlas(las)
  assert_all_are_non_negative(res)

  if (length(res) == 1L)
    res <- c(res,res)
  else if (length(res) > 2L)
    stop("Wrong resolution provided.")

  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)

  call <- lazyeval::as_call(func)
  by   <- group_grid_3d(las@data$X, las@data$Y, las@data$Z, res, c(0,0,0.5*res[2]))
  stat <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = by]
  data.table::setnames(stat, c("Xgrid", "Ygrid", "Zgrid"), c("X", "Y", "Z"))
  data.table::setattr(stat, "class", c("lasmetrics3d", attr(stat, "class")))
  data.table::setattr(stat, "res", res)
  return(stat)
}


