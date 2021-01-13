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
#' @param func formula. An expression to be applied to each voxel (see also \link{grid_metrics}).
#' @param res numeric. The resolution of the voxels. \code{res = 1} for a 1x1x1 cubic voxels. Optionally
#' \code{res = c(1,2)} for non-cubic voxels (1x1x2 cuboid voxel).
#' @param ... Unused
#' @param all_voxels boolean. By default the function returns only voxels that
#' contain 1 or more points. Empty voxels do not exist as the metrics are undefined.
#' If \code{all_voxels = TRUE} all the voxels are returned and metrics are NA for
#' voxels with 0 points.
#'
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table
#' has the class \code{lasmetrics3d} enabling easier plotting. It also has an
#' attribute \code{res} that stores the resolution.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # Cloud of points is voxelized with a 8-meter resolution and in each voxel
#' # the number of points is computed.
#' vm <- voxel_metrics(las, ~length(Z), 8)
#'
#' # Cloud of points is voxelized with a 8-meter resolution and in each voxel
#' # the mean intensity of points is computed.
#' vm <- voxel_metrics(las, ~mean(Intensity), 8)
#' #plot(vm, color = "V1", colorPalette = heat.colors(50), trim = 60)
#'
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
#' voxels <- voxel_metrics(las, ~myMetrics(Intensity), 8)
#'
#' #plot(voxels, color = "imean", colorPalette = heat.colors(50), trim = 60)
#' #etc.
#'
#' attr(voxels, "res")
#' @family metrics
voxel_metrics = function(las, func, res = 1, ..., all_voxels = FALSE)
{
  stopifnotlas(las)
  assert_all_are_non_negative(res)
  assert_is_a_bool(all_voxels)

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

  if (all_voxels)
  {
    xrange <- range(las$X)
    yrange <- range(las$Y)
    zrange <- range(las$Z)
    xrange <- f_grid(xrange, res[1], 0)
    yrange <- f_grid(yrange, res[1], 0)
    zrange <- f_grid(zrange, res[2], 0.5*res[2])
    X <- seq(xrange[1], xrange[2], by = res[1])
    Y <- seq(yrange[1], yrange[2], by = res[1])
    Z <- seq(zrange[1], zrange[2], by = res[2])
    all <- expand.grid(X = X,Y = Y, Z = Z)
    data.table::setDT(all)
    data.table::setkey(all, X,Y, Z)
    data.table::setkey(stat, X,Y, Z)
    stat <- stat[all]
  }

  data.table::setattr(stat, "class", c("lasmetrics3d", attr(stat, "class")))
  data.table::setattr(stat, "res", res[1])

  return(stat)
}


