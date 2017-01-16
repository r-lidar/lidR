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



#' Rasterize the space and compute metrics for each cell
#'
#' Computes a series of descriptive statistics for a LiDAR dataset within each cell
#' of a grid.
#'
#' Computes a series of descriptive statistics defined by the user. Output is a
#' data.frame in which each line is a raster (single grid cell), and each column is a metric.
#' grid_metrics is similar to \code{lasmetrics} except it computes metrics within each cell
#' in a predefinded grid. The grid cell coordinates are pre-determined for a given resolution.
#' So the algorithm will always provide the same coordinates independently of the dataset.
#' When start = (0,0) and res = 20 grid_metrics will produce the following raster centers:
#' (10,10), (10,30), (30,10) etc.. When start = (-10, -10) and res = 20 grid_metrics will
#' produce the following raster centers: (0,0), (0,20), (20,0) etc.. In Quebec (Canada) reference
#' is (-831600,  117980) in the NAD83 coordinate system. The function to be applied to each
#' cell is a classical function (see examples) that returns a labelled list of metrics.
#' The following existing function can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics}}
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:LAD]{LAD}}
#' } Users must write their own functions to create metrics. \code{grid_metrics} will
#' dispatch the LiDAR data for each cell in the user's function. The user writes their
#' function without considering grid cells, only a cloud of points (see example).
#'
#' @param .las An object of class \code{LAS}
#' @param func the function to be applied to each cell
#' @param res numeric. The size of the cells. Default 20.
#' @param start vector x and y coordinates for the reference raster. Default is (0,0).
#' @param splitlines logical. If TRUE the algorithm will compute the metrics for each
#' flightline individually. It returns the same cells several times in overlap.
#' @return It returns a \code{data.table} containing the metrics for each cell. The table
#' has the class "lasmetrics" enabling easy plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' grid_metrics(lidar, max(Z), 2) %>% plot
#'
#' # Mean height with 400 m^2 cells
#' grid_metrics(lidar, mean(Z)) %>% plot
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
#' metrics = grid_metrics(lidar, myMetrics(Z, Intensity))
#'
#' plot(metrics, "zwimean")
#' plot(metrics, "zimean")
#' plot(metrics, "zsqmean")
#' #etc.
#' @export
grid_metrics = function(.las, func, res = 20, start = c(0,0), splitlines = FALSE)
{
  stopifnotlas(.las)

  func_call = substitute(func)

  if(is(func_call, "name"))
    func_call = eval(func_call)

  .las@data %$% eval(func_call) %>% .debug_grid_metrics(func_call)

  by = group_grid(.las@data$X, .las@data$Y, res, start)

  if(splitlines & "flightlineID" %in% names(.las@data))
    by = c(by, list(flightline = .las@data$flightlineID))
  else if(splitlines & !"flightlineID" %in% names(.las@data))
    lidRError("LDR7")

  stat <- .las@data[, c(eval(func_call)), by = by]

  n = names(stat)
  n[1:2] = c("X", "Y")

  data.table::setnames(stat, n)
  data.table::setattr(stat, "class", c("lasmetrics", attr(stat, "class")))
  data.table::setattr(stat, "res", res)

  return(stat)
}

.debug_grid_metrics = function(metrics, func)
{
  funcstring = deparse(func)

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    if(is.null(names(metrics)))
      names(metrics) = paste0("#", 1:length(metrics))

    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical", "character")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) == 1)
      lidRError("TFS1", expression = funcstring, metric = n, class = c)
    else if(sum(!test) > 1)
      lidRError("TFS2", expression = funcstring, metric = n, class = c)

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) == 1)
      lidRError("TFS3", expression = funcstring, metric = n, number = c)
    else if(sum(!test) > 1)
      lidRError("TFS4", expression = funcstring, metric = n, number = c)
  }
  else if(is.data.frame(metrics))
    lidRError("TFS5", expression = funcstring)
  else if(is.vector(metrics) & length(metrics) > 1)
    lidRError("TFS6", expression = funcstring, number = length(metrics))
  else
    return(0)
}
