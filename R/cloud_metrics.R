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
#' \code{cloud_metrics} computes a series of user-defined descriptive statistics for a LiDAR dataset.
#' See \link[=grid_metrics]{grid_metrics} to compute metrics on a grid. Basically there are
#' no predefined metrics. Users must write their own functions to create metrics (see example).
#' The following existing functions can serve as a guide to help users compute their own metrics:
#' \itemize{
#' \item{\link[=stdmetrics]{stdmetrics}}
#' \item{\link[=entropy]{entropy}}
#' \item{\link[=VCI]{VCI}}
#' \item{\link[=LAD]{LAD}}
#' }
#' @param las An object of class \code{LAS}
#' @param func formula. An expression to be applied to the point cloud (see example)
#' @return It returns a \code{list} containing the metrics
#' @export
#' @seealso
#' \link[=grid_metrics]{grid_metrics}
#' \link[=stdmetrics]{stdmetrics}
#' \link[=entropy]{entropy}
#' \link[=VCI]{VCI}
#' \link[=LAD]{LAD}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' cloud_metrics(lidar, ~max(Z))
#' cloud_metrics(lidar, ~mean(Intensity))
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
#' metrics = cloud_metrics(lidar, ~myMetrics(Z, Intensity))
#'
#' # Predefined metrics
#' cloud_metrics(lidar, .stdmetrics)
#' @family metrics
cloud_metrics = function(las, func)
{
  UseMethod("cloud_metrics", las)
}

#' @export
cloud_metrics.LAS = function(las, func)
{
  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  func      <- lazyeval::f_interp(func)
  call      <- lazyeval::as_call(func)
  metric    <- with(las@data, eval(call))
  return(metric)
}

#' @export
cloud_metrics.LAScluster = function(las, func)
{
  las <- readLAS(las)
  if (is.empty(las)) return(NULL)
  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  return(cloud_metrics(las, func))
}

