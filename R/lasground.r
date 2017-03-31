# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https:#github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
#
# This file is part of lidRExtra R package.
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
# along with this program.  If not, see <http:#www.gnu.org/licenses/>
#
# ===============================================================================


#' Classify points as ground or not ground
#'
#' Implements a Progressive Morphological Filter for segmentation of ground points.
#' The function updates the field \code{Classification} of the input LAS obect. The points
#' classified as 'ground' are assigned a value of 2 according to las specifications (See the ASPRS
#' documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#' This function is an implementation of the Zhang et al. (2003) algorithm (see reference)
#'
#' @param .las a LAS object
#' @param MaxWinSize numeric. Maximum window size to be used in filtering ground returns (see references)
#' @param Slope  numeric. Slope value to be used in computing the height thresholds (see references)
#' @param InitDist numeric. Initial height above the parameterized ground surface to be considered a ground return (see references)
#' @param MaxDist numeric. Maximum height above the parameterized ground surface to be considered a ground return (see references)
#' @param CellSize numeric. Cell size
#' @param ... Any additional specific parameters to be passed to the progressive morphological filter.
#' These include:\cr
#' - \code{exponential} logical. Default is TRUE.\cr
#' - \code{base} numeric. Default is 2\cr
#' @return Nothing. The original LAS object is updated by reference. In the 'Classification'
#' column a value of 2 denotes ground according to LAS specifications.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, XYZonly = TRUE)
#'
#' lasground(las, MaxWinSize = 40, Slope = 1, MaxDist = 5, InitDist = 0.01, CellSize = 7)
#'
#' plot(las, color = "Classification")
#' @importFrom data.table :=
lasground = function(.las, MaxWinSize = 20, Slope = 1.0, InitDist = 0.5, MaxDist = 3.0, CellSize = 1.0, ...)
{
  . <- X <- Y <- Z <- Classification <- NULL

  dots = list(...)

  if(is.null(dots$base)) dots$base = 2
  if(is.null(dots$exponential)) dots$exponential = TRUE

  exponential = dots$exponential
  base = dots$base

  cloud = .las@data[, .(X,Y,Z)]
  cloud[, idx := 1:dim(cloud)[1]]

  if("Classification" %in% names(.las@data))
  {
    nground = fast_countequal(.las@data$Classification, 2)

    if(nground > 0)
    {
      warning(paste0("Orginal dataset already contains ", nground, " ground points. These points were reclassified as 'unclassified' before to perform a new ground classification."), call. = FALSE)
      .las@data[Classification == 2, Classification := 0]
    }
  }
  else
  {
    .las@data[, Classification := 0]
  }

  idx = ProgressiveMorphologicalFilter(cloud, MaxWinSize, Slope, InitDist, MaxDist, CellSize, base, exponential)

  message(paste(length(idx), "ground points found."))

  .las@data[idx, Classification := 2]

  return(invisible())
}

ProgressiveMorphologicalFilter = function(cloud, MaxWinSize, Slope, InitDist, MaxDist, CellSize, base, exponential)
{
  # Compute the series of window sizes and height thresholds
  height_thresholds = c()
  window_sizes = c()
  iteration = 0
  window_size = 0
  height_threshold = 0

  while (window_size <= MaxWinSize)
  {
    # Determine the initial window size.
    if (exponential)
      window_size = CellSize * ( (2.0 * base^iteration) + 1 )
    else
      window_size = CellSize * (2.0 * (iteration+1) * base + 1)

    # Calculate the height threshold to be used in the next iteration.
    if (iteration == 0)
      height_threshold = InitDist
    else
      height_threshold = Slope * (window_size - window_sizes[iteration]) * CellSize + InitDist

    # Enforce max distance on height threshold
    if (height_threshold > MaxDist)
      height_threshold = MaxDist

    window_sizes = append(window_sizes, window_size)
    height_thresholds = append(height_thresholds, height_threshold)

    iteration = iteration + 1
  }

  # Filter ground returns using a progressive morphological filter
  for (i in 1:length(window_sizes))
  {
    Z_f = MorphologicalOpening(cloud$X, cloud$Y, cloud$Z, window_sizes[i])

    # Find indices of the points whose difference between the source and
    # filtered point clouds is less than the current height threshold
    diff = cloud$Z - Z_f
    indices = diff < height_thresholds[i]

    # Limit filtering to those points currently considered ground returns
    cloud = cloud[indices]
  }

  return(cloud$idx)
}
