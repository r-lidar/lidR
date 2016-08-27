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
#' Computes a series of descriptive statistics for a LiDAR dataset
#'
#' Computes a series of descriptive statistics for a LiDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set. See \link[lidR:gridmetrics]{gridmetrics}
#' to compute metrics on a grid. Basically there are no predifined metrics. Users
#' must write their own function to create metrics (see example). The following existing
#' function can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:canopyMatrix]{canopyMatrix}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:canopyClosure]{canopyClosure}}
#' \item{\link[lidR:fractal_dimension]{fractal_dimension}}
#' \item{\link[lidR:LAD]{LAD}}
#' }
#' @aliases cloudMetrics
#' @param obj An object of class \code{LAS}
#' @param func The function to be applied to a cloud of points
#' @return It returns a \code{data.table} containing the metrics
#' @export cloudMetrics
#' @seealso \link[lidR:gridmetrics]{gridmetrics}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' cloudMetrics(lidar, max(Z))
#' cloudMetrics(lidar, mean(Z))
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
#' metrics = cloudMetrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID))
#' @importFrom magrittr %$%
setGeneric("cloudMetrics", function(obj, func){standardGeneric("cloudMetrics")})

#' @rdname cloudMetrics
setMethod("cloudMetrics", "LAS",
	function(obj, func)
	{
	  func_call = substitute(func)
	  metric = obj@data %$% eval(func_call)
		return(metric)
	}
)
