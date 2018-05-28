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
#' Thin LIDAR data randomly removes a given proportion of points to reach specific point/pulse densities.
#'
#' \code{lasfilterdecimate} is designed to produce output datasets that have uniform densities
#' throughout the coverage area. For each cell, the proportion of points/pulses that will
#' be retained is computed using the actual density and the desired density. If the required density
#' is greater than the actual density it returns an unchanged set of points (it cannot increase the
#' density). If \code{homogenize = FALSE} is selected, it randomly removes points/pulses to reach the
#' required density over the whole area (see \code{\link[lidR:area]{area}}). The cell size must be large
#' enough to compute a coherent local pulse density i.e., in a 2 points/m^2 dataset, 25 square meters
#' would be feasible; however, an extent too small to thin (e.g. <1 square meter) would not be feasible
#' because density does not have meaning at this scale.
#'
#' @param .las An object of the class \code{LAS}
#' @param density numeric. The expected density
#' @param homogenize logical. If \code{TRUE}, the algorithm tries to homogenize the pulse density to
#' provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density over the whole
#' area.
#' @param res numeric. Cell size to compute the pulse density.
#' @param use_pulse logical. Decimate by removing random pulses instead of random points
#' @return It returns a \code{LAS} object.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile, select = "xyz")
#'
#' # By default the method is homogenize = TRUE
#' thinned = lasfilterdecimate(lidar, 1, res = 5)
#' plot(grid_density(lidar))
#' plot(grid_density(thinned))
#'
#' # Method homogenize = FALSE enables a global pulse density to be reached
#' thinned = lasfilterdecimate(lidar, 1, homogenize = FALSE)
#' summary(thinned)
#' d = grid_density(thinned)
#' plot(d)
#' @export
lasfilterdecimate = function(.las, density, homogenize = TRUE, res = 5, use_pulse = FALSE)
{
  stopifnotlas(.las)
  assertive::assert_is_a_number(density)
  assertive::assert_all_are_positive(density)
  assertive::assert_is_a_bool(homogenize)
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)
  assertive::assert_is_a_bool(use_pulse)

  pulseID <- gpstime <- NULL

  if(use_pulse & !"pulseID" %in% names(.las@data))
  {
    warning("No 'pulseID' field found.", call. = FALSE)
    use_pulse = FALSE
  }

  npoints = nrow(.las@data)

  if(homogenize == FALSE)
  {
    n = round(density*area(.las))

    if (use_pulse)
      selected = selected_pulses(.las@data$pulseID, n)
    else
      selected = sample(1:nrow(.las@data), n)
  }
  else
  {
    n = round(density*res^2)

    by = group_grid(.las@data$X, .las@data$Y, res)

    if (use_pulse)
      selected = .las@data[, .I[selected_pulses(pulseID, n)], by = by]$V1
    else
      selected = .las@data[, .I[selected_pulses(1:.N, n)], by = by]$V1
  }

  return(LAS(.las@data[selected], .las@header, .las@crs))
}

selected_pulses = function(pulseID, n)
{
  p = unique(pulseID)

  if(n > length(p))
    return(rep(TRUE, length(pulseID)))

  selectedPulses = sample(p, n)
  selectedPulses = pulseID %in% selectedPulses

  return(selectedPulses)
}
