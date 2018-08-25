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



#' Thin LiDAR data
#'
#' This routine creates a grid with a given resolution and filters the point cloud by selecting randomly
#' some point in each cell. It is designed to produce output datasets that have uniform densities throughout
#' the coverage area. For each cell, the proportion of points/pulses that will be retained is computed
#' using the actual density and the desired density. If the required density is greater than the actual
#' density it returns an unchanged set of points (it cannot increase the' density). If \code{homogenize = FALSE}
#' is selected, it randomly removes points/pulses to reach the required density over the whole area (see
#' \code{\link[lidR:area]{area}}). The cell size must be large enough to compute a coherent local pulse
#' density i.e., in a 2 points/m^2 dataset, 25 square meters would be feasible; however, an extent too
#' small to thin (e.g. <1 square meter) would not be feasible because density does not have meaning
#' at this scale.
#'
#' @template section-supported-option-lasfilter
#'
#' @template LAScatalog
#'
#' @template param-las
#' @param density numeric. The expected density
#' @param homogenize logical. If \code{TRUE}, the algorithm tries to homogenize the pulse density to
#' provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density over the whole
#' area.
#' @param res numeric. Cell size to compute the pulse density.
#' @param use_pulse logical. Decimate by removing random pulses instead of random points (requieres to run
#' \link{laspulse} first)
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # By default the method is homogenize = TRUE
#' thinned = lasfilterdecimate(las, 1, res = 5)
#' plot(grid_density(las))
#' plot(grid_density(thinned))
#'
#' # Method homogenize = FALSE enables a global pulse density to be reached
#' thinned = lasfilterdecimate(las, 1, homogenize = FALSE)
#' summary(thinned)
#' d = grid_density(thinned)
#' plot(d)
#' @export
#' @family lasfilters
lasfilterdecimate = function(las, density, homogenize = TRUE, res = 5, use_pulse = FALSE)
{
  assertive::assert_is_a_number(density)
  assertive::assert_all_are_positive(density)
  assertive::assert_is_a_bool(homogenize)
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)
  assertive::assert_is_a_bool(use_pulse)

  UseMethod("lasfilterdecimate", las)
}

#' @export
lasfilterdecimate.LAS = function(las, density, homogenize = TRUE, res = 5, use_pulse = FALSE)
{
  pulseID <- gpstime <- NULL

  if(use_pulse & !"pulseID" %in% names(las@data))
  {
    warning("No 'pulseID' field found.", call. = FALSE)
    use_pulse <- FALSE
  }

  npoints <- nrow(las@data)

  selected_pulses = function(pulseID, n)
  {
    p <- unique(pulseID)

    if(n > length(p))
      return(rep(TRUE, length(pulseID)))

    selectedPulses <- sample(p, n)
    selectedPulses <- pulseID %in% selectedPulses

    return(selectedPulses)
  }

  if(homogenize == FALSE)
  {
    n <- round(density*area(las))

    if (use_pulse)
      selected <- selected_pulses(las@data$pulseID, n)
    else
      selected <- sample(1:nrow(las@data), n)
  }
  else
  {
    n  <- round(density*res^2)
    by <- group_grid(las@data$X, las@data$Y, res)

    if (use_pulse)
      selected <- las@data[, .I[selected_pulses(pulseID, n)], by = by]$V1
    else
      selected <- las@data[, .I[selected_pulses(1:.N, n)], by = by]$V1
  }

  return(LAS(las@data[selected], las@header, las@proj4string))
}

#' @export
lasfilterdecimate.LAScluster = function(las, density, homogenize = TRUE, res = 5, use_pulse = FALSE)
{
  x <- readLAS(las)
  if (is.null(x)) return(NULL)
  x <- lasfilterdecimate(x, density, homogenize, res, use_pulse)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasfilterdecimate.LAScatalog = function(las, density, homogenize = TRUE, res = 5, use_pulse = FALSE)
{
  buffer(las) <- 0.1*res
  las@input_options$select <- "*"

  output      <- catalog_apply2(las, lasfilterdecimate, density = density, homogenize = homogenize, res = res, use_pulse = use_pulse, need_buffer = FALSE, check_alignement = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}
