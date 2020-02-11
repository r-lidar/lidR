# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' Area-Based Approach in hexagonal cells.
#'
#' Computes a series of descriptive statistics for a LiDAR dataset within hexagonal cells.
#' This function is identical to \link{grid_metrics} but with hexagonal cells instead of
#' square pixels. After all, we conduct circular plot inventories and we map models on pixel-based maps.
#' \code{hexbin_metrics} provides the opportunity to test something else. Refer to \link{grid_metrics}
#' for more information.
#'
#' @param las An object of class \code{LAS}.
#'
#' @param func formula. An expression to be applied to each hexagonal cell.
#'
#' @param res numeric. To be consistent with \link{grid_metrics}, the square of \code{res} give the area
#' of the hexagonal cells, like in \code{grid_metrics}. The difference being the fact that for square pixels this
#' is obvious. Here \code{res = 20} gives 400-square-meter hexagonal cells.
#'
#' @return A \link[hexbin:hexbin-class]{hexbin} object from package \code{hexbin} or a \code{list} of
#' \code{hexbin} objects if several metrics are returned.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' col = grDevices::colorRampPalette(c("blue", "cyan2", "yellow", "red"))
#'
#' # Maximum elevation with a resolution of 8 m
#' hm = hexbin_metrics(lidar, ~max(Z), 8)
#' hexbin::plot(hm, colramp = col, main = "Max Z")
#'
#' # Mean height with a resolution of 20 m
#' hm = hexbin_metrics(lidar, ~mean(Z), 20)
#' hexbin::plot(hm, colramp = col, main = "Mean Z")
#'
#' # Define your own new metrics
#' myMetrics = function(z, i)
#' {
#'   metrics = list(
#'     zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#'     zimean  = mean(z*i),       # Mean products of z by intensity
#'   zsqmean = sqrt(mean(z^2))    # Quadratic mean
#'   )
#'
#'   return(metrics)
#' }
#'
#' metrics = hexbin_metrics(lidar, ~myMetrics(Z, Intensity), 10)
#'
#' hexbin::plot(metrics$zwimean, colramp = col, main = "zwimean")
#' hexbin::plot(metrics$zimean, colramp = col, main = "zimean")
#' hexbin::plot(metrics$zsqmean, colramp = col, main = "zsqmean")
#' @family metrics
hexbin_metrics = function(las, func, res = 20)
{
  stopifnotlas(las)

  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)

  call <- lazyeval::as_call(func)

  if (!requireNamespace("hexbin", quietly = TRUE))
    stop("'hexbin' package is needed for this function to work. Please install it.")

  res  <- round(sqrt(((2*res*res)/(3*sqrt(3)))), 2)
  ext  <- raster::extent(las)
  xmin <- round_any(ext@xmin, res)
  xmax <- round_any(ext@xmax, res)
  ymin <- round_any(ext@ymin, res)
  ymax <- round_any(ext@ymax, res)

  if (xmax < ext@xmax) xmax <- xmax + res
  if (xmin > ext@xmin) xmin <- xmin - res
  if (ymax < ext@ymax) ymax <- ymax + res
  if (ymin > ext@ymin) ymin <- ymin - res

  dx    <- (xmax - xmin)
  dy    <- (ymax - ymin)
  xbins <- (xmax - xmin)/(2*res)

  layout   <- hexbin::hexbin(las@data$X, las@data$Y, shape = dy/dx,  xbins = xbins, xbnds = c(xmin, xmax), IDs = TRUE)
  metrics  <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = layout@cID]
  data.table::setorder(metrics, layout)
  metrics[, layout := NULL]
  layout@cID = 0L
  layout@xlab = "X"
  layout@ylab = "Y"

  n <- ncol(metrics)
  output <- vector(mode = "list", n)
  names(output) <- names(metrics)

  for (i in 1:n)
  {
    hexbin = layout
    hexbin@count = metrics[[i]]
    output[[i]] = hexbin
  }

  if (n > 1)
    return(output)
  else
    return(output[[1]])
}
