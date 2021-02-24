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


#' Map the pulse or point density
#'
#' Creates a map of the point density. If a "pulseID" attribute is found, also returns a map of the pulse
#' density.
#'
#' @template param-las
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 = 16
#' square meters.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-LayerBrick
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile,  filter = "-inside 684800 5017800 684900 5017900")
#'
#' d <- grid_density(las, 5)
#' plot(d)
#'
#' las <- retrieve_pulses(las)
#' d <- grid_density(las)
#' plot(d)
grid_density = function(las, res = 4)
{
  if (is(res, "RasterLayer"))
    resolution = raster::res(res)[1]
  else
    resolution = res

  if (!"pulseID" %in% names(las@data))
    X <- grid_metrics(las, ~list(point_density = .N), res)
  else
    X <- grid_metrics(las, ~list(point_density = .N, pulse_density = length(unique(pulseID))), res)

  X[is.na(X)] <- 0
  return(X/resolution^2)
}
