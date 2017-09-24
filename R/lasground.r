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
#' Implements algorithms for segmentation of ground points. The function updates the field
#' \code{Classification} of the input LAS object. The points classified as 'ground' are
#' assigned a value of 2 according to las specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#'
#' @param las a LAS object
#' @param algorithm character. The name of an algorithm. Currently \code{"pmf"} is supported
#' (see related sections)
#' @param ... Parameter for each avaible algorithm
#' @param min_ws numeric. Minimum windows size to be used in filtering ground returns.
#' This is the parameter \code{b} in Zhang et al. (2003) (see references)
#' @param max_ws numeric. Maximum window size to be used in filtering ground returns in
#' Zhang et al. (2003)
#' @param min_th numeric. Initial threshold height above the parameterized ground surface
#' to be considered a ground return. This is \code{dt0} in Zhang et al. (2003)
#' @param max_th numeric. Maximum threshold height above the parameterized ground surface
#' to be considered a ground return. This is \code{dhmax} in Zhang et al. (2003)
#' @param s  numeric. Slope value to be used in computing the height thresholds sequence.
#'  This is the parameter \code{s} in Zhang et al. (2003)
#'
#' @section Progressive morphological filter (PMF):
#'
#' This method is an implementation of the Zhang et al. (2003) algorithm (see reference).
#' This is not a strict implementation of Zhang et al. This algorithm works at the raw point
#' cloud level without any rasterization process. Therefore some parameter from the original
#' description are no longer useful. The cell size \code{c} is not a requiered parameter.
#' Therefore in the original paper the threshold sequence was:\cr\cr
#' \eqn{th_k = s*(w_k - w_{k-1})*c + th_0}\cr\cr
#' Now it is:\cr\cr
#' \eqn{th_k = s*(w_k - w_{k-1}) + th_0}\cr\cr
#' This method support an extra logical parameters \code{exponential} to change the method
#' used to create the windows size sequence. Default is FALSE. See reference.
#'
#' @return Nothing. The original LAS object is updated by reference. In the 'Classification'
#' column a value of 2 denotes ground according to LAS specifications.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' lasground(las, "pmf", min_ws = 2, max_ws = 40, min_th = 0.01, max_th = 5, slope = 1)
#'
#' plot(las, color = "Classification")
#' @importFrom data.table :=
lasground = function(las, algorithm, ...)
{
  if (algorithm == "pmf")
    lasground_pmf(las, ...)
  else
    stop("This algorithm does not exist.", call. = F)
}

#' @rdname lasground
#' @export
lasground_pmf = function(las, min_ws = 2, max_ws = 20, min_th = 0.5, max_th = 3.0, s = 1.0, ...)
{
  . <- X <- Y <- Z <- Classification <- NULL

  stopifnotlas(las)

  dots = list(...)

  if (is.null(dots$exponential)) dots$exponential = FALSE

  exponential = dots$exponential

  cloud = las@data[, .(X,Y,Z)]
  cloud[, idx := 1:dim(cloud)[1]]

  if ("Classification" %in% names(las@data))
  {
    nground = fast_countequal(las@data$Classification, 2)

    if (nground > 0)
    {
      warning(paste0("Orginal dataset already contains ", nground, " ground points. These points were reclassified as 'unclassified' before to perform a new ground classification."), call. = FALSE)
      las@data[Classification == 2, Classification := 0]
    }
  }
  else
  {
    las@data[, Classification := 0]
  }

  verbose("Progressive morphological filter...")

  idx = ProgressiveMorphologicalFilter(cloud, min_ws, max_ws, min_th, max_th, s, exponential)

  message(paste(length(idx), "ground points found."))

  las@data[idx, Classification := 2]

  return(invisible())
}

ProgressiveMorphologicalFilter = function(cloud, InitWinSize, MaxWinSize, InitDist, MaxDist, Slope, exponential)
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
      window_size = ((2.0 * InitWinSize^iteration) + 1)
    else
      window_size = (2.0 * (iteration + 1) * InitWinSize + 1)

    # Calculate the height threshold to be used in the next iteration.
    if (iteration == 0)
      height_threshold = InitDist
    else
      height_threshold = Slope * (window_size - window_sizes[iteration]) + InitDist

    # Enforce max distance on height threshold
    if (height_threshold > MaxDist)
    {
      height_threshold = MaxDist
    }

    if (window_size <= MaxWinSize)
    {
      window_sizes = append(window_sizes, window_size)
      height_thresholds = append(height_thresholds, height_threshold)
    }

    iteration = iteration + 1
  }

  verbose("Windows sizes: ", window_sizes)
  verbose("Height thresholds: ", height_thresholds)

  # Filter ground returns using a progressive morphological filter
  for (i in 1:length(window_sizes))
  {
    verbose(paste0("Pass ", i, " of ", length(window_sizes), "..."))
    verbose(paste0("Windows size = ", window_sizes[i], " ; height_threshold = ", height_thresholds[i]))

    Z_f = MorphologicalOpening(cloud$X, cloud$Y, cloud$Z, window_sizes[i], LIDROPTIONS("progress"))

    # Find indices of the points whose difference between the source and
    # filtered point clouds is less than the current height threshold
    diff = cloud$Z - Z_f
    indices = diff < height_thresholds[i]

    # Limit filtering to those points currently considered ground returns
    cloud = cloud[indices]
  }

  return(cloud$idx)
}
