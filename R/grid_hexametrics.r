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



#' Compute metrics for hexagonal cells
#'
#' Computes a series of descriptive statistics for a LiDAR dataset within hexagonal cells
#' from a hexagonal grid pattern. This function is identical to \link{grid_metrics} or
#' \link{grid_metrics3d} or \link{tree_metrics} but with hexagonal cells instead of classical
#' square pixels. Please refer to \link{grid_metrics} for more information.
#'
#' @param .las An object of class \code{LAS}
#' @param func the function to be applied to each hexagonal cell
#' @param res numeric. The inscribed circle radius of a hexagon. Default = 20.
#' @param splitlines logical. If TRUE the algorithm will compute the metrics for each
#' flightline individually. It returns the same cells several times in overlaps.
#' @param debug logical. If you encounter a non trivial error try \code{debug = TRUE}.
#' @return It returns a \code{data.table} containing the metrics for each hexagonal cell. The table
#' has the class "lashexametrics" enabling easy plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Maximum elevation with a resolution of 4 m
#' hm = grid_hexametrics(lidar, max(Z), 4)
#' plot(hm)
#'
#' # Mean height with a resolution of 20 m
#' hm = grid_hexametrics(lidar, mean(Z))
#' plot(hm)
#'
#' # Define your own new metrics
#' myMetrics = function(z, i)
#' {
#'   metrics = list(
#'      zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#'      zimean  = mean(z*i),       # Mean products of z by intensity
#'      zsqmean = sqrt(mean(z^2))  # Quadratic mean
#'    )
#'
#'    return(metrics)
#' }
#'
#' metrics = grid_hexametrics(lidar, myMetrics(Z, Intensity), 10)
#'
#' plot(metrics, "zwimean")
#' plot(metrics, "zimean")
#' plot(metrics, "zsqmean")
#' #etc.
#' @export
grid_hexametrics = function(.las, func, res = 20, splitlines = FALSE, debug = FALSE)
{
  stopifnotlas(.las)
  call <- substitute(func)
  stat <- lasaggregate(.las, by = "HEXA", call, res, NA, c("X", "Y"), splitlines, debug)
  return(stat)
}
