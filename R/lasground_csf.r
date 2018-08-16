# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https:#github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' Classify points as ground based on Cloth Simulation
#'
#' Implements algorithms for segmentation of ground points. The function updates the field
#' \code{Classification} of the LAS input object. The points classified as 'ground' are
#' assigned a value of 2 according to las specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#' This method is the a strict implementation of the CSF algorithm made by Zhang et al. (2016) (see
#' references) that relies on the orginal source code written by the original author and exposed to
#' R via the the \code{RCSF} package.
#'
#' @param las a LAS object.
#' @param sloop_smooth logical. When sharp slopes exist, set this parameter to TRUE to perform a
#' post-processing which will reduced errors.
#' @param class_threshold scalar. The distance to the simulated cloth to classify point cloud into ground
#' and non-ground. The default is 0.5.
#' @param cloth_resolution scalar. The distance between paticles in cloth. This is usually set to the
#' average distance of the points in the point cloud. The default value is 0.5.
#' @param rigidness integer. The rididness of the cloth. 1 stands for very soft cloth (to fit rugged
#' terrain), 2 stands for medium cloth and 3 stands for hard cloth (for flat terrain). The default is 1.
#' @param iterations integer. Maximum iteration for simulating cloth. The default value is 500. Usually,
#' users do not need to change this.
#' @param time_step scalar. Time step when simulating the cloth under the gravity. The default value
#' is 0.65. Usually, do not change this value. It is suitable for most cases.
#' @param last_returns logical. The algorithm will use only the last returns (including the first returns
#' in the cases of single return) to run the algorithm. If FALSE all the returns are used. If the fields
#' \code{'ReturnNumber'} or \code{'NumberOfReturns'} are not specified \code{'last_returns'} is turned
#' to \code{FALSE} automatically.
#'
#' @template return-lasground
#'
#' @references
#' W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne LiDAR Data
#' Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501, 2016.
#' (http://www.mdpi.com/2072-4292/8/6/501/htm)
#'
#' @export
#' @family lasground
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyzrn")
#'
#' lasground(las, "csf")
#'
#' plot(las, color = "Classification")
lasground_csf = function(las, sloop_smooth = FALSE, class_threshold = 0.5, cloth_resolution = 0.5, rigidness = 1L, iterations = 500L, time_step = 0.65, last_returns = TRUE)
{
  assertive::assert_is_a_bool(sloop_smooth)
  assertive::assert_is_a_number(class_threshold)
  assertive::assert_is_a_number(cloth_resolution)
  assertive::assert_is_a_number(rigidness)
  assertive::assert_all_are_whole_numbers(rigidness)
  assertive::assert_is_a_number(iterations)
  assertive::assert_all_are_whole_numbers(iterations)
  assertive::assert_is_a_number(time_step)

  . <- X <- Y <- Z <- Classification <- NULL

  npoints <- nrow(las@data)
  filter  <- !logical(npoints)
  pointID <- 1:npoints

  if (last_returns)
  {
    n <- names(las@data)

    if (!all(c("ReturnNumber", "NumberOfReturns") %in% n))
      warning("'ReturnNumber' and/or 'NumberOfReturns' not found. Cannot use the option 'last_returns', all the points were used", call. = FALSE)
    else
      filter = las@data$ReturnNumber == las@data$NumberOfReturns

    if(sum(filter) == 0)
      stop("0 last return found. Process aborted.", call. = FALSE)
  }

  cloud <- las@data[filter, .(X,Y,Z)]
  cloud[, idx := pointID[filter]]

  gnd <- RCSF:::R_CSF(cloud, sloop_smooth, class_threshold, cloth_resolution, rigidness, iterations, time_step)
  idx <- cloud$idx[gnd]

  message(glue::glue("{length(idx)} ground points found."))

  if ("Classification" %in% names(las@data))
  {
    nground = fast_countequal(las@data$Classification, 2)

    if (nground > 0)
    {
      warning(glue::glue("Orginal dataset already contains {nground} ground points. These points were reclassified as 'unclassified' before to perform a new ground classification."), call. = FALSE)
      las@data[Classification == 2, Classification := 0]
    }
  }
  else
  {
    las@data[, Classification := 0L]
  }

  las@data[idx, Classification := 2L]

  return(invisible())
}