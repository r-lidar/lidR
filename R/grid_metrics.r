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



#' Area Based Approach
#'
#' Computes a series of user-defined descriptive statistics for a LiDAR dataset within
#' each pixel of a raster (area-based approach). The grid cell coordinates are pre-determined for a
#' given resolution, so the algorithm will always provide the same coordinates independently of the
#' dataset. When start = (0,0) and res = 20 grid_metrics will produce the following cell centers:
#' (10,10), (10,30), (30,10) etc. aligning the corner of a cell on (0,0). When start = (-10, -10) and
#' res = 20' grid_metrics will produce the following cell centers: (0,0), (0,20), (20,0) etc. aligning
#' the corner of a cell on (-10, -10).
#'
#' @section Parameter \code{func}:
#' The function to be applied to each cell is a classical function (see examples) that
#' returns a labelled list of metrics. For exemple the following function \code{f} is correctly formed.
#' \preformatted{
#' f = function(x) {list(mean = mean(x), max = max(x))}
#' }
#' And could be applied either on the \code{Z} coordinates or the Intensities for exemples. These two
#' statements are valid:
#' \preformatted{
#' grid_metrics(las, f(Z), res = 20)
#' grid_metrics(las, f(Intensity), res = 20)
#' }
#' The following existing functions allow the user to
#' compute some predefined metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics}}
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:VCI]{VCI}}
#' \item{\link[lidR:LAD]{LAD}}
#' }
#' But usually the users must write their own functions to create metrics. \code{grid_metrics} will
#' dispatch the point cloud in the user's function.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template param-las
#' @param func the function to be applied to each cell (see section "Parameter func")
#' @param res numeric. The size of the cells. Default 20.
#' @param start vector x and y coordinates for the reference raster. Default is (0,0) meaning that the
#' grid aligns on (0,0).
#'
#' @template return-grid-LayerBrick
#'
#' @examples
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' colors = height.colors(50)
#'
#' # Canopy surface model with 4 m^2 cells
#' metrics = grid_metrics(las, max(Z), 2)
#' plot(metrics, col = colors)
#'
#' # Mean height with 400 m^2 cells
#' metrics = grid_metrics(las, mean(Z), 20)
#' plot(metrics, col = colors)
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
#' metrics = grid_metrics(las, myMetrics(Z, Intensity))
#'
#' plot(metrics, col = colors)
#' plot(metrics, "zwimean", col = colors)
#' plot(metrics, "zimean", col = colors)
#' plot(metrics, "zsqmean", col = colors)
#' @export
grid_metrics = function(las, func, res = 20, start = c(0,0))
{
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_non_negative(res)
  assertive::assert_is_numeric(start)

  UseMethod("grid_metrics", las)
}

#' @export
grid_metrics.LAS = function(las, func, res = 20, start = c(0,0))
{
  . <- X <- Y <- NULL

  call = substitute(func)
  if (call == "func") call = func
  if (is.name(call)) call = parse(text = eval(call))

  layout    <- make_overlay_raster(las, res, start)
  cells     <- raster::cellFromXY(layout, las@data[, .(X,Y)])
  metrics   <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = cells]

  if (ncol(metrics) == 2L)
  {
    suppressWarnings(layout[metrics[[1]]] <- metrics[[2]])
    names(layout) <- names(metrics)[2]
    return(layout)
  }
  else
  {
    xy_coords <- raster::xyFromCell(layout, metrics[[1]])
    metrics[, cells := NULL]
    output <- sp::SpatialPixelsDataFrame(xy_coords, metrics, proj4string = las@crs)
    return(raster::brick(output))
  }
}

#' @export
grid_metrics.LAScluster = function(las, func, res = 20, start = c(0,0))
{
  x = readLAS(las)
  if (is.null(x)) return(NULL)
  bbox = raster::extent(as.numeric(las@bbox))
  metrics = grid_metrics(x, func, res, start)
  metrics = raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
grid_metrics.LAScatalog = function(las, func, res = 20, start = c(0,0))
{
  buffer(las)   <- 0.1*res
  output        <- catalog_apply2(las, grid_metrics, func = substitute(func), res = res, start = start, need_buffer = FALSE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (output_files(las) != "") return(unlist(output))

  # Outputs have been return in R objects. Merge the outptus in a single object
  names         <- names(output[[1]])
  factor        <- output[[1]]@data@isfactor
  output        <- do.call(raster::merge, output)
  output@crs    <- las@proj4string
  names(output) <- names
  if (is(output, "RasterBrick")) colnames(output@data@values) <- names
  return(output)
}

