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



#' Thin LiDAR data
#'
#' Thin LIDAR data randomly removes a given proportion of pulses to reach specific pulse densities
#'
#' lasthin is designed to produce output data sets that have uniform pulse densities
#' throughout the coverage area. For each cell, the proportion of pulses that will
#' be retained is computed using the calculated pulse density and the desired pulse
#' density. If required pulse density is greater than the local pulse density it returns
#' an unchanged set of points (it cannot increase the pulse density). In the way
#' of \code{homogenize = FALSE} it randomly removes pulses to reach the required pulse
#' density on the whole area (see \code{\link[lidR:lasarea]{lasarea}}). The cell size must be large enough
#' to compute a coherant local pulse density. 25 square meters looks good. 1 square
#' meter is meaningless.
#' @param .las An object of the class \code{LAS}
#' @param density numeric. The expected density
#' @param homogenize logical. If \code{TRUE}, the algorithm tries to homogenize the pulse density to provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density on the whole area.
#' @param resolution numeric. Cell size to compute the pulse density.
#' @return It returns a \code{LAS} object.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # By default the method is homogenize = TRUE
#' thinned = lidar %>% lasthin(1, resolution = 5)
#' lidar   %>% grid_density %>% plot
#' thinned %>% grid_density %>% plot
#'
#' # Method homogenize = FALSE enables a global pulse density to be reached
#' thinned = lidar %>% lasthin(1, homogenize = FALSE)
#' thinned %>% summary
#' thinned %>% grid_density %>% plot
#' @export
lasthin = function(.las, density, homogenize = TRUE, resolution = 5)
{
  pulseID <- gpstime <- NULL

  stopifnotlas(.las)

  if(! "pulseID" %in% names(.las@data))
    lidRError("THI1")

  if(homogenize == FALSE)
  {
    n = round(density*.las@area)
    selected = .selectPulseToRemove(.las@data$pulseID, n)
  }
  else
  {
    n = round(density*resolution^2)

    x_raster = round_any(.las@data$X, resolution)
    y_raster = round_any(.las@data$Y, resolution)

    by = list(Xr = x_raster,Yr = y_raster)

    selected = .las@data[, list(delete = .selectPulseToRemove(pulseID, n), t = gpstime), by=by]
    selected[, c("Xr", "Yr") := NULL]

    setorder(selected, t)
    setorder(.las@data, gpstime)

    selected = selected$delete
  }

  LAS(.las@data[selected], .las@header) %>% return()
}


.selectPulseToRemove = function(pulseID, n)
{
  p = unique(pulseID)

  if(n > length(p))
    return(rep(TRUE, length(pulseID)))

  selectedPulses = sample(p, n)
  selectedPulses = pulseID %in% selectedPulses

  return(selectedPulses)
}
