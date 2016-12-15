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



#' Canopy surface model
#'
#' Creates a canopy surface model using a LiDAR point cloud. For each pixel the
#' function returns the highest point found. This basic method could be improved by replacing
#' each LiDAR return with a small disk.
#'
#' @aliases  grid_canopy
#' @param .las An object of class \code{LAS}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is
#' 2 meters i.e. 4 square meters.
#' @param subcircle numeric radius of the circles. To fill empty pixels the algorithm
#' replaces each return by a circle composed of 8 points before computing the maximum elevation
#' in each pixel.
#' @return It returns a \code{data.table} with the class \code{lasmetrics} which enables easier plotting and
#' RasterLayer casting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Local maximum algorithm with a resolution of 2 meters
#' lidar %>% grid_canopy(2) %>% plot
#'
#' # Local maximum algorithm with a resolution of 1 meters
#' lidar %>% grid_canopy(1) %>% plot
#'
#' # Local maximum algorithm with a resolution of 1 meters replacing each
#' # point by a 20 cm radius circle of 8 points
#' lidar %>% grid_canopy(1, 0.2) %>% plot
#' @family grid_alias
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:as.raster.lasmetrics]{as.raster}
#' @export grid_canopy
grid_canopy = function(.las, res = 2, subcircle = 0)
{
  . <- X <- Y <- Z <- NULL

  if(subcircle > 0)
  {
    ex = extent(.las)

    dt = .las@data[, .(X,Y,Z)]

    alpha = seq(0, 2*pi, length.out = 9)[-9]
    px = subcircle*cos(alpha)
    py = subcircle*sin(alpha)

    dt = dt[, subcircled(X,Y,Z, px,py), by = rownames(dt)][, rownames := NULL]
    dt = dt[between(X, ex@xmin, ex@xmax) & between(Y, ex@ymin, ex@ymax)]
    .las = LAS(dt)

    rm(dt)
  }

  ret = grid_metrics(.las, list(Z = max(Z)), res)

  rm(.las)
  gc()

  return(ret)
}

subcircled = function(x, y, z, px, py)
{
  i = which.max(z)
  x = x[i]
  y = y[i]
  z = z[i]

  x = x + px
  y = y + py
  z = rep(z, length(px))

  list(X = x, Y = y, Z = z)
}
