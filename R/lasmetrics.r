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



#' Compute metrics for a cloud of points
#'
#' \code{lasmetrics} computes a series of user-defined descriptive statistics for a LiDAR dataset.
#' See \link[lidR:grid_metrics]{grid_metrics} to compute metrics on a grid. Basically there are
#' no predefined metrics. Users must write their own functions to create metrics (see example).
#' The following existing functions can serve as a guide to help users compute their own metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics}}
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:LAD]{LAD}}
#' }
#' @param obj An object of class \code{LAS}
#' @param func The function to be applied to a cloud of points. Function must return a \code{list} (see example)
#' @return It returns a \code{list} containing the metrics
#' @export
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:stdmetrics]{stdmetrics}
#' \link[lidR:entropy]{entropy}
#' \link[lidR:VCI]{VCI}
#' \link[lidR:LAD]{LAD}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' lasmetrics(lidar, max(Z))
#' lasmetrics(lidar, mean(ScanAngle))
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
#' metrics = lasmetrics(lidar, myMetrics(Z, Intensity))
#'
#' # Predefined metrics
#' lasmetrics(lidar, .stdmetrics)
lasmetrics = function(obj, func)
{
  func_call = substitute(func)

  if(is(func_call, "name"))
    func_call = eval(func_call)

  metric = with(obj@data, eval(func_call))
  return(metric)
}

