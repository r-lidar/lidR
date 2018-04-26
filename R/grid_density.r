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
#' Creates a map of the point density. If a "pulseID" field is found, return also a map of the pulse
#' density.
#'
#' @section Use with a \code{LAScatalog}:
#' When the parameter \code{x} is a \link[lidR:LAScatalog-class]{LAScatalog} the function processes
#' the entire dataset in a continuous way using a multicore process. The user can modify the processing
#' options using the \link[lidR:catalog]{available options}.\cr\cr
#' \code{lidR} support .lax files. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @aliases grid_density
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a LAScatalog")
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 = 16
#' square meters.
#' @param filter character. Streaming filter while reading the files (see \link{readLAS}).
#' If \code{x} is a \code{LAScatalog} the function \link{readLAS} is called internally. The
#' user cannot manipulate the lidar data directly but can use streaming filters instead.
#' @return Returns a \code{data.table} of class \code{lasmetrics} which enables easier
#' plotting and RasterLayer casting.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' d = grid_density(lidar, 5)
#' plot(d)
#' d = grid_density(lidar, 10)
#' plot(d)
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
    ret = grid_metrics(x, list(point_density = length(X)/res^2), res)
  }
  else
  {
    ret = grid_metrics(x, list(point_density = .N/res^2, pulse_density = length(unique(pulseID))/res^2), res)
  }

  return(ret)
}

#' @export
grid_density.LAScatalog = function(x, res = 4, filter = "")
{
  x = catalog_old_compatibility(x)

  buffer(x) <- res/2
  ret <- grid_catalog(x, grid_density, res, "xyzt", filter)
  return(ret)
}
