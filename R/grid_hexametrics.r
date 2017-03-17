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
#' from a hexagonal grid pattern. This function is identical to \link{grid_metrics}
#' but with hexagonal cells instead of classical square pixels. Please refer to
#' \link{grid_metrics} for more information.
#'
#' @param .las An object of class \code{LAS}
#' @param func the function to be applied to each hexagonal cell
#' @param res numeric. The inscribed circle radius of a hexagon. Default = 20.
#' @param splitlines logical. If TRUE the algorithm will compute the metrics for each
#' flightline individually. It returns the same cells several times in overlaps.
#' @return It returns a \code{data.table} containing the metrics for each hexagonal cell. The table
#' has the class "lashexametrics" enabling easy plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Maximum elevation with a resolution of 4 m
#' grid_hexametrics(lidar, max(Z), 4) %>% plot
#'
#' # Mean height with a resolution of 20 m
#' grid_hexametrics(lidar, mean(Z)) %>% plot
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
grid_hexametrics = function(.las, func, res = 20, splitlines = FALSE)
{
  if (!requireNamespace("hexbin", quietly = TRUE))
    stop("'hexbin' package is needed for this function to work. Please install it.", call. = F)

  stopifnotlas(.las)

  func_call = substitute(func)

  if(is(func_call, "name"))
    func_call = eval(func_call)

  .las@data %$% eval(func_call) %>% .debug_grid_metrics(func_call)

  ext = extent(.las)
  xmin = round_any(ext@xmin, res)
  xmax = round_any(ext@xmax, res)
  if(xmax < ext@xmax) xmax = xmax + res
  if(xmin > ext@xmin) xmin = xmin - res

  xbins = (xmax - xmin)/res

  hbin_data  = hexbin::hexbin(.las@data$X, .las@data$Y, xbins = xbins, xbnds = c(xmin, xmax), IDs = TRUE)
  hbin_coord = hexbin::hcell2xy(hbin_data)
  hbin_ids   = hbin_data@cID
  hbin_pos   = cumsum(1:max(hbin_data@cell) %in% hbin_data@cell)
  hbin_pos_ids = hbin_pos[hbin_ids]

  by = list(Xr = hbin_coord$x[hbin_pos_ids], Yr = hbin_coord$y[hbin_pos_ids])

  if(splitlines & "flightlineID" %in% names(.las@data))
    by = c(by, list(flightline = .las@data$flightlineID))
  else if(splitlines & !"flightlineID" %in% names(.las@data))
    lidRError("LDR7")

  stat <- .las@data[, c(eval(func_call)), by = by]

  n = names(stat)
  n[1:2] = c("X", "Y")

  data.table::setnames(stat, n)
  data.table::setattr(stat, "class", c("lashexametrics", attr(stat, "class")))
  data.table::setattr(stat, "res", res)

  return(stat)
}
