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



#' Map the pulse or point density
#'
#' Creates a pulse density map using a LiDAR cloud of points. This function is an alias
#' for \code{grid_metrics(obj, f, res)} with \code{f} = \code{length(unique(pulseID))/res^2)}
#'
#' @section Use with a \code{Catalog}:
#' When the parameter \code{x} is a catalog the function processes the entiere dataset
#' in a continuous way using a multicore process. Parallel computing is set by defaut to
#' the number of core avaible in the computer. No buffer is requiered. The user can modify
#' the global options using the function \link{catalog_options}.
#'
#' @aliases grid_density
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a Catalog")
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 = 16 square meters.
#' @param filter character. Streaming filter while reading the files (see \link{readLAS}).
#' If \code{x} is a \code{Catalog} the function \link{readLAS} is called internally. The
#' user cannot manipulate the lidar data himself but can use streaming filters instead.
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
grid_density = function(x, res = 4, filter = "")
{
  UseMethod("grid_density", x)
}


#' @export
grid_density.LAS = function(x, res = 4, filter = "")
{
  pulseID <- density <- X <- NULL

  if(! "pulseID" %in% names(x@data))
  {
    warning("No column named pulseID found. The pulse density cannot be computed. Computing the point density instead of the pulse density.", call. = F)
    ret = grid_metrics(x, list(density = length(X)/res^2), res)
  }
  else
    ret = grid_metrics(x, list(density = length(unique(pulseID))/res^2), res)

  return(ret)
}

#' @export
grid_density.Catalog = function(x, res = 4, filter = "-drop_z_below 0")
{
  ret <- grid_catalog(x, grid_density, res, "xyztP", filter)

  return(ret)
}
