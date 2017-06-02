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



#' Pulse density surface model
#'
#' Creates a pulse density map using a LiDAR cloud of points. This function is an alias
#' for \code{grid_metrics(obj, f, res} with \code{f} = \code{length(unique(pulseID))/res^2)}
#'
#' @section Use with a \code{Catalog}:
#' When the parameter \code{x} is a catalog the function will process the entiere dataset
#' in a continuous way using a multicore process. Parallel computing is set by defaut to
#' the number of core avaible in the computer. No buffer is requiered. The user can modify
#' the global options using the function \link{catalog_options}.
#'
#' @aliases grid_density
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a Catalog")
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 = 16 square meters.
#' @return It returns a \code{data.table} of the class \code{lasmetrics} which enables easier plotting and
#' RasterLayer casting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' lidar %>% grid_density(5) %>% plot
#' lidar %>% grid_density(10) %>% plot
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' @export
grid_density = function(x, res = 4)
{
  pulseID <- density <- X <- NULL

  if (is(x, "LAS"))
  {
    if(! "pulseID" %in% names(x@data))
    {
      warning("No column named pulseID found. The pulse density cannot be computed. Compute the point density instead of the pulse density.", call. = F)
      ret = grid_metrics(x, list(density = length(X)/res^2), res)
    }
    else
      ret = grid_metrics(x, list(density = length(unique(pulseID))/res^2), res)
  }
  else if(is(x, "Catalog"))
  {
    ret <- grid_catalog(x, grid_density, res, "", 0, FALSE)
  }
  else
  {
    xtxt = lazyeval::expr_text(x)
    stop(paste0(xtxt, " is not neither a LAS nor a Catalog object."), call. = FALSE)
  }

  return(ret)
}
