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

#' Ground Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_ground}. It implements an algorithm for segmentation
#' of ground points based on a progressive morphological filter. This method is an implementation of
#' the Zhang et al. (2003) algorithm (see reference). Note that this is not a strict implementation
#' of Zhang et al. This algorithm works at the point cloud level without any rasterization process.
#' The morphological operator is applied on the point cloud, not on a raster. Also, Zhang et al.
#' proposed some formulas (eq. 4, 5 and 7) to compute the sequence of windows sizes and thresholds.
#' Here, these parameters are free and specified by the user. The function \link{util_makeZhangParam}
#' enables computation of the parameters according to the original paper.
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
#'
#' @family ground segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#'
#' ws <- seq(3,12, 3)
#' th <- seq(0.1, 1.5, length.out = length(ws))
#'
#' las <- classify_ground(las, pmf(ws, th))
#' plot(las, color = "Classification")
pmf = function(ws, th)
{
  ws <- lazyeval::uq(ws)
  th <- lazyeval::uq(th)

  f = function(las, filter)
  {
    assert_is_valid_context(LIDRCONTEXTGND, "pmf")
    return(C_pmf(las, ws, th, filter))
  }

  class(f) <- LIDRALGORITHMGND
  return(f)
}

#' Ground Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_ground}. It implements an algorithm for segmentation
#' of ground points base on a Cloth Simulation Filter. This method is a strict implementation of
#' the CSF algorithm made by Zhang et al. (2016) (see references) that relies on the authors' original
#' source code written and exposed to R via the the \code{RCSF} package.
#'
#' @param sloop_smooth logical. When steep slopes exist, set this parameter to TRUE to reduce
#' errors during post-processing.
#' @param class_threshold scalar. The distance to the simulated cloth to classify a point cloud into ground
#' and non-ground. The default is 0.5.
#' @param cloth_resolution scalar. The distance between particles in the cloth. This is usually set to the
#' average distance of the points in the point cloud. The default value is 0.5.
#' @param rigidness integer. The rigidness of the cloth. 1 stands for very soft (to fit rugged
#' terrain), 2 stands for medium, and 3 stands for hard cloth (for flat terrain). The default is 1.
#' @param iterations integer. Maximum iterations for simulating cloth. The default value is 500. Usually,
#' there is no need to change this value.
#' @param time_step scalar. Time step when simulating the cloth under gravity. The default value
#' is 0.65. Usually, there is no need to change this value. It is suitable for most cases.
#'
#' @references
#' W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne LiDAR Data
#' Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501, 2016.
#' (http://www.mdpi.com/2072-4292/8/6/501/htm)
#'
#' @export
#'
#' @family ground segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#'
#' mycsf <- csf(TRUE, 1, 1, time_step = 1)
#' las <- classify_ground(las, mycsf)
#' plot(las, color = "Classification")
csf = function(sloop_smooth = FALSE, class_threshold = 0.5, cloth_resolution = 0.5, rigidness = 1L, iterations = 500L, time_step = 0.65)
{
  sloop_smooth     <- lazyeval::uq(sloop_smooth)
  class_threshold  <- lazyeval::uq(class_threshold)
  cloth_resolution <- lazyeval::uq(cloth_resolution)
  rigidness        <- lazyeval::uq(rigidness)
  iterations       <- lazyeval::uq(iterations)
  time_step        <- lazyeval::uq(time_step)

  f = function(las, filter)
  {
    . <- X <- Y <- Z <- NULL
    assert_is_valid_context(LIDRCONTEXTGND, "csf")
    las@data[["idx"]] <- 1:npoints(las)
    cloud <- las@data[filter, .(X,Y,Z, idx)]
    gnd <- RCSF:::R_CSF(cloud, sloop_smooth, class_threshold, cloth_resolution, rigidness, iterations, time_step)
    idx <- cloud$idx[gnd]
    return(idx)
  }

  class(f) <- LIDRALGORITHMGND
  return(f)
}


#' Parameters for progressive morphological filter
#'
#' The function \link{classify_ground} with the progressive morphological filter allows for any
#' sequence of parameters. This function enables computation of the sequences using equations (4),
#'  (5) and (7) from Zhang et al. (see reference and details).
#' @details
#' In the original paper the windows size sequence is given by eq. 4 or 5:\cr\cr
#'
#' \eqn{w_k = 2kb + 1} \cr\cr
#' or\cr\cr
#' \eqn{w_k = 2b^k + 1}\cr\cr
#'
#' In the original paper the threshold sequence is given by eq. 7:\cr\cr
#' \eqn{th_k = s*(w_k - w_{k-1})*c + th_0}\cr\cr
#' Because the function \link{classify_ground} applies the morphological operation at the point
#' cloud level the parameter \eqn{c} is set to 1 and cannot be modified.
#' @param b numeric. This is the parameter \eqn{b} in Zhang et al. (2003) (eq. 4 and 5).
#' @param max_ws numeric. Maximum window size to be used in filtering ground returns. This limits
#' the number of windows created.
#' @param dh0 numeric. This is \eqn{dh_0} in Zhang et al. (2003) (eq. 7).
#' @param dhmax numeric. This is \eqn{dh_{max}} in Zhang et al. (2003) (eq. 7).
#' @param s numeric. This is \eqn{s} in Zhang et al. (2003) (eq. 7).
#' @param exp logical. The window size can be increased linearly or exponentially (eq. 4 or 5).
#' @return A list with two components: the windows size sequence and the threshold sequence.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#' @export
#'
#' @examples
#' p = util_makeZhangParam()
util_makeZhangParam = function(b = 2, dh0 = 0.5, dhmax = 3.0, s = 1.0,  max_ws = 20, exp = FALSE)
{
  if (exp & b <= 1)
    stop("b cannot be less than 1 with an exponentially growing window")

  if (dh0 >= dhmax)
    stop("dh0 greater than dhmax")

  if (max_ws < 3)
    stop("Minimum windows size is 3. max_ws must be greater than 3")

  if (!is.logical(exp))
    stop("exp should be logical")

  if (!exp & b < 1)
    warning("Due to an incoherence in the original paper when b < 1, the sequences of windows size cannot be computed for a linear increase. The internal routine uses the fact that the increment is constant to bypass this issue.")


  dhtk = c()
  wk = c()
  k = 0
  ws = 0
  th = 0
  c = 1

  while (ws <= max_ws)
  {
    # Determine the initial window size.
    if (exp)
      ws = (2.0*b^k) + 1
    else
      ws = 2.0*(k + 1)*b + 1

    # Calculate the height threshold to be used in the next k.
    if (ws <= 3)
      th = dh0
    else
    {
      if (exp)
        th = s * (ws - wk[k]) * c + dh0
      else
        th = s*2*b*c + dh0
    }

    # Enforce max distance on height threshold
    if (th > dhmax)
      th = dhmax

    if (ws <= max_ws)
    {
      wk = append(wk, ws)
      dhtk = append(dhtk, th)
    }

    k = k + 1
  }

  return(list(ws = wk, th = dhtk))
}
