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
#' Computes a series of user-defined descriptive statistics for a LiDAR dataset within
#' each pixel of a raster. Output is a data.table in which each line is a pixel (single grid cell),
#' and each column is a metric. Works both with \link{LAS} or \link{catalog} objects.
#' \code{grid_metrics} is similar to \link{lasmetrics} or \link{grid_hexametrics} except it
#' computes metrics within each cell in a predefined grid. The grid cell coordinates are
#' pre-determined for a given resolution.
#'
#' \code{grid_metrics} is similar to \link{lasmetrics} or \link{grid_hexametrics} except it
#' computes metrics within each cell in a predefined grid. The grid cell coordinates are
#' pre-determined for a given resolution, so the algorithm will always provide the same coordinates
#' independently of the dataset. When start = (0,0) and res = 20 grid_metrics will produce the
#' following raster centers: (10,10), (10,30), (30,10) etc.. When start = (-10, -10) and res = 20
#' grid_metrics will produce the following raster centers: (0,0), (0,20), (20,0) etc.. In Quebec
#' (Canada) the reference is (-831600,  117980) in the NAD83 coordinate system.
#'
#' @section Parameter \code{func}:
#' The function to be applied to each cell is a classical function (see examples) that
#' returns a labelled list of metrics. The following existing functions allows the user to
#' compute some metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics}}
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:LAD]{LAD}}
#' } Users must write their own functions to create metrics. \code{grid_metrics} will
#' dispatch the LiDAR data for each cell in the user's function. The user writes their
#' function without considering grid cells, only a point cloud (see example).
#'
#' @section Parameter \code{start}:
#' The algorithm will always provide the same coordinates independently of the dataset.
#' When start = (0,0) and res = 20 grid_metrics will produce the following raster centers:
#' (10,10), (10,30), (30,10) etc..  When start = (-10, -10) and res = 20 grid_metrics will
#' produce the following raster centers: (0,0), (0,20), (20,0) etc.. In Quebec (Canada)
#' reference is (-831600,  117980) in the NAD83 coordinate system.
#'
#' @section Use with a \code{LAScatalog}:
#' When the parameter \code{x} is a \link[lidR:LAScatalog-class]{LAScatalog} the function processes
#' the entire dataset in a continuous way using a multicore process. Parallel computing is set
#' by default to the number of core available in the computer. The user can modify the global
#' options using the function \link{catalog_options}.\cr\cr
#' \code{lidR} support .lax files. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a LAScatalog")
#' @param func the function to be applied to each cell (see section "Parameter func")
#' @param res numeric. The size of the cells. Default 20.
#' @param start vector x and y coordinates for the reference raster. Default is (0,0) (see section "Parameter start").
#' @param splitlines logical. If TRUE the algorithm will compute the metrics for each
#' flightline individually. It returns the same cells several times in overlap.
#' @param filter character. Streaming filter while reading the files (see \link{readLAS}).
#' If the input is a \code{LAScatalog} the function \link{readLAS} is called internally. The
#' user cannot manipulate the lidar data directly but can use streaming filters instead.
#' @return Returns a \code{data.table} containing the metrics for each cell. The table
#' has the class "lasmetrics" enabling easy plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' grid_metrics(lidar, max(Z), 2) %>% plot
#'
#' # Mean height with 400 m^2 cells
#' grid_metrics(lidar, mean(Z), 20) %>% plot
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
#' plot(metrics)
#' plot(metrics, "zwimean")
#' plot(metrics, "zimean")
#' plot(metrics, "zsqmean")
#' #etc.
#' @export
grid_metrics = function(x, func, res = 20, start = c(0,0), splitlines = FALSE, filter = "")
{
  UseMethod("grid_metrics", x)
}

#' @export
grid_metrics.LAS = function(x, func, res = 20, start = c(0,0), splitlines = FALSE, filter = "")
{
  call = substitute(func)

  stat <- lasaggregate(x, by = "XY", call, res, start, c("X", "Y"), splitlines)

  return(stat)
}

#' @export
grid_metrics.LAScatalog = function(x, func, res = 20, start = c(0,0), splitlines = FALSE, filter = "")
{
  call = substitute(func)

  if (any(start != 0))  warning("Parameter start is currently disabled for LAScatalogs")
  if (splitlines)       warning("Parameter splitlines is currently disabled for LAScatalogs")

  oldbuffer <- CATALOGOPTIONS("buffer")
  CATALOGOPTIONS(buffer = 0)

  stat <- grid_catalog(x, grid_metrics, res, "*+", filter, func = call)

  CATALOGOPTIONS(buffer = oldbuffer)

  return(stat)
}
