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
#' Computes a series of user-defined descriptive statistics for a LiDAR dataset
#'
#' Computes a series of descriptive statistics for a LiDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set. See \link[lidR:grid_metrics]{grid_metrics}
#' to compute metrics on a grid. Basically there are no predifined metrics. Users
#' must write their own functions to create metrics (see example). The following existing
#' example functions can serve as a guide to help users compute their own metrics:
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:stdmetrics]{stdmetrics}}
#' }
#' @aliases cloud_metrics
#' @param obj An object of class \code{LAS}
#' @param func The function to be applied to a cloud of points
#' @return It returns a \code{data.table} containing the metrics
#' @export cloud_metrics
#' @seealso \link[lidR:grid_metrics]{grid_metrics}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' cloud_metrics(lidar, max(Z))
#' cloud_metrics(lidar, mean(Z))
#'
#' # Define your own metric function
#' myMetrics = function(z, i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         hmean   = mean(z),
#'         hmax    = max(z),
#'         imean   = mean(i),
#'         angle   = mean(abs(angle))
#'         )
#'
#'    return(ret)
#'  }
#'
#' metrics = cloud_metrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID))
cloud_metrics = function(obj, func)
{
  func_call = substitute(func)

  if(is(func_call, "name"))
    func_call = eval(func_call)

  metric = obj@data %$% eval(func_call)
  return(metric)
}
