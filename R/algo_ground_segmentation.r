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

#' Progressive Morphological Filter
#'
#' This function is made to be used in \link{lasground}. It implements an algorithms for segmentation
#' of ground points. This method is an implementation of the Zhang et al. (2003) algorithm (see reference).
#' Note that this is not a strict implementation of Zhang et al. This algorithm works at the point
#' cloud level without any rasterization process. The morphological operator is applied on the point
#'cloud, not on a raster. Also, Zhang et al. proposed some formulas (eq. 4, 5 and 7) to compute the
#' sequence of windows sizes and thresholds. Here, these parameters are free and specified by the user.
#' The function \link{util_makeZhangParam} enables computation of the parameters according to the
#' original paper.
#'
#' @param ws numeric. Sequence of windows sizes to be used in filtering ground returns.
#' The values must be positive and in the same units as the point cloud (usually meters, occasionally
#' feet).
#' @param th numeric. Sequence of threshold heights above the parameterized ground surface to be
#' considered a ground return. The values must be positive and in the same units as the point cloud.
#'
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#'
#' @export
#' @family Algorithm
#' @family Ground Segmentation
pmf = function(ws, th)
{
  f = function(cloud)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasground"), "pmf")

    for (i in 1:length(ws))
    {
      verbose(glue::glue("Pass {i} of {length(ws)}..."))
      verbose(glue::glue("Windows size = {ws[i]} ; height_threshold = {th[i]}"))

      Z_f = C_MorphologicalOpening(cloud$X, cloud$Y, cloud$Z, ws[i])

      # Find indices of the points whose difference between the source and
      # filtered point clouds is less than the current height threshold
      diff = cloud$Z - Z_f
      indices = diff < th[i]

      # Limit filtering to those points currently considered ground returns
      cloud = cloud[indices]
    }

    idx <- cloud$idx
  }

  class(f) <- c("GroundSegmentation", "Algorithm", "lidR")
  return(f)
}

#' Cloth Simulation Filter
#'
#' This function is made to be used in \link{lasground}. It implements an algorithms for segmentation
#' of ground points. This method is the a strict implementation of the CSF algorithm made by Zhang
#' et al. (2016) (see references) that relies on the orginal source code written by the original
#' author and exposed to R via the the \code{RCSF} package.
#'
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
#'
#' @references
#' W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne LiDAR Data
#' Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501, 2016.
#' (http://www.mdpi.com/2072-4292/8/6/501/htm)
#'
#' @export
#' @family Algorithm
#' @family Ground Segmentation
csf = function(sloop_smooth = FALSE, class_threshold = 0.5, cloth_resolution = 0.5, rigidness = 1L, iterations = 500L, time_step = 0.65)
{
  f = function(cloud)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lasground"), "csf")

    gnd <- RCSF:::R_CSF(cloud, sloop_smooth, class_threshold, cloth_resolution, rigidness, iterations, time_step)
    idx <- cloud$idx[gnd]
  }

  class(f) <- c("GroundSegmentation", "Algorithm", "lidR")
  return(f)
}