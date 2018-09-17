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



#' Point Cloud Decimation Algorithm
#'
#' This function is made to be used in \link{lasfilterdecimate}. It implements an algorithms that
#' randomly removes points or pulses to reach the desired density over the whole area (see
#' \code{\link[lidR:area]{area}}).
#'
#' @param density numeric. The desired output density.
#'
#' @param use_pulse logical. Decimate by removing random pulses instead of random points (requieres to run
#' \link{laspulse} first)
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Reach a pulse density of 1 on the overall dataset
#' thinned1 = lasfilterdecimate(las, random(1))
#' plot(grid_density(las))
#' plot(grid_density(thinned1))
random = function(density, use_pulse = FALSE)
{
  assertive::assert_is_a_number(density)
  assertive::assert_all_are_positive(density)
  assertive::assert_is_a_bool(use_pulse)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasfilterdecimate"), "random")

    if(use_pulse & !"pulseID" %in% names(las@data))
    {
      warning("No 'pulseID' attribute found. Decimation by points is used.", call. = FALSE)
      use_pulse <- FALSE
    }

    n <- round(density*area(las))

    if (use_pulse)
      return(.selected_pulses(las@data$pulseID, n))
    else
    {
      if (nrow(las@data) > n)
        return(sample(1:nrow(las@data), n))
      else
        return(1:nrow(las@data))
    }
  }

  class(f) <- c("function", "PointCloudDecimation", "Algorithm", "lidR")
  return(f)
}

#' Point Cloud Decimation Algorithm
#'
#' This function is made to be used in \link{lasfilterdecimate}. It implements an algorithms that
#' creates a grid with a given resolution and filters the point cloud by selecting randomly
#' some point in each cell. It is designed to produce point clouds that have uniform densities throughout
#' the coverage area. For each cell, the proportion of points or pulses that will be retained is computed
#' using the actual local density and the desired density. If the desired density is greater than the actual
#' density it returns an unchanged set of points (it cannot increase the density). The cell size must be
#' large enough to compute a coherent local density. For example in a 2 points/m^2 point cloud, 25 square
#' meters would be feasible; however 1 square meter cells would not be feasible because density does
#' not have meaning at this scale.
#'
#' @param density numeric. The desired output density.
#'
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @param use_pulse logical. Decimate by removing random pulses instead of random points (requieres to run
#' \link{laspulse} first)
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Select point randomly to reach an homogeneous density of 1
#' thinned = lasfilterdecimate(las, homogenize(1,5))
#' plot(grid_density(thinned))
homogenize = function(density, res = 5, use_pulse = FALSE)
{
  assertive::assert_is_a_number(density)
  assertive::assert_all_are_positive(density)
  assertive::assert_is_a_bool(use_pulse)
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasfilterdecimate"), "homogenize")

    if(use_pulse & !"pulseID" %in% names(las@data))
    {
      warning("No 'pulseID' attribute found. Decimation by points is used.", call. = FALSE)
      use_pulse <- FALSE
    }

    pulseID <- NULL

    n  <- round(density*res^2)
    by <- group_grid(las@data$X, las@data$Y, res)

    if (use_pulse)
      return(las@data[, .I[.selected_pulses(pulseID, n)], by = by]$V1)
    else
      return(las@data[, .I[.selected_pulses(1:.N, n)], by = by]$V1)
  }

  class(f) <- c("function", "PointCloudDecimation", "Algorithm", "lidR")
  return(f)
}

#' Point Cloud Decimation Algorithm
#'
#' This function is made to be used in \link{lasfilterdecimate}. It implements an algorithms that
#' creates a grid with a given resolution and filters the point cloud by selecting the highest point
#' within each cell.
#'
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Select the highest point within each cell of an overlayed grid
#' thinned = lasfilterdecimate(las, highest(4))
#' plot(thinned)
highest = function(res = 1)
{
  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasfilterdecimate"), "highest")

    Z   <- NULL
    by  <- group_grid(las@data$X, las@data$Y, res)
    return(las@data[, .I[which.max(Z)], by = by]$V1)
  }

  class(f) <- c("function", "PointCloudDecimation", "Algorithm", "lidR")
  return(f)
}

.selected_pulses = function(pulseID, n)
{
  p <- unique(pulseID)

  if(n > length(p))
    return(rep(TRUE, length(pulseID)))

  selectedPulses <- sample(p, n)
  selectedPulses <- pulseID %in% selectedPulses

  return(selectedPulses)
}