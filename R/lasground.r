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
#' \code{Classification} of the LAS input object. The points classified as 'ground' are
#' assigned a value of 2 according to las specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#'
#' @param las a LAS object.
#' @param algorithm character. The name of an algorithm. Currently \code{"pmf"} is supported
#' (see related sections).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' of the algorithms).
#' @param ws numeric. Sequence of windows sizes to be used in filtering ground returns.
#' The values must be positive and in the units of the point cloud (usually meters, occasionally feet).
#' @param th numeric. Sequence of threshold heights above the parameterized ground surface
#' to be considered a ground return. The values must be positive and are in the units of
#' the point cloud (usually meters, occasionally feet).
#'
#' @section Progressive morphological filter (PMF):
#'
#' This method is an implementation of the Zhang et al. (2003) algorithm (see reference).
#' This is not a strict implementation of Zhang et al. This algorithm works at the point
#' cloud level without any rasterization process. The morphological operator is applied on
#' the point cloud not on a raster. Also Zhang et al. proposed some formulas (eq. 4, 5 and 7)
#' to compute the sequence of windows sizes and thresholds. Here these parameters are free
#' and specified by the user. The function \link{util_makeZhangParam} enables computation
#' of the parameters according to the original paper.
#'
#' @return Nothing. The original LAS object is updated by reference. In the 'Classification'
#' column a value of 2 denotes 'ground' according to LAS specifications.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' ws = seq(3,21, 3)
#' th = seq(0.1, 2, length.out = length(ws))
#'
#' lasground(las, "pmf", ws, th)
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
lasground_pmf = function(las, ws, th)
{
  . <- X <- Y <- Z <- Classification <- NULL

  lws = length(ws)
  lth = length(th)

  if (!is.vector(ws)) {stop("'ws' is not a vector.", call. = FALSE)}
  if (!is.vector(th)) {stop("'th' is not a vector.", call. = FALSE)}
  if (!is.numeric(ws)){stop("'ws' is not numeric", call. = FALSE)}
  if (!is.numeric(th)){stop("'th' is not numeric.", call. = FALSE)}
  if (lws != lth)     {stop("'ws' and 'th' are not the same length.", call. = FALSE)}
  if (lws == 0)       {stop("'ws' is empty.", call. = FALSE)}
  if (lth == 0)       {stop("'th' is empty.", call. = FALSE)}
  if (any(ws <= 0))   {stop("'ws' contains negative or null values.", call. = FALSE)}
  if (any(th <= 0))   {stop("'th' contains negative or null values.", call. = FALSE)}
  if (any(is.na(ws))) {stop("'ws' contains NA values.", call. = FALSE)}
  if (any(is.na(th))) {stop("'th' contains NA values.", call. = FALSE)}

  stopifnotlas(las)

  cloud = las@data[, .(X,Y,Z)]
  cloud[, idx := 1:dim(cloud)[1]]

  verbose("Progressive morphological filter...")

  for (i in 1:lws)
  {
    verbose(paste0("Pass ", i, " of ", length(ws), "..."))
    verbose(paste0("Windows size = ", ws[i], " ; height_threshold = ", th[i]))

    Z_f = MorphologicalOpening(cloud$X, cloud$Y, cloud$Z, ws[i], LIDROPTIONS("progress"))

    # Find indices of the points whose difference between the source and
    # filtered point clouds is less than the current height threshold
    diff = cloud$Z - Z_f
    indices = diff < th[i]

    # Limit filtering to those points currently considered ground returns
    cloud = cloud[indices]
  }

  idx = cloud$idx

  message(paste(length(idx), "ground points found."))

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

  las@data[idx, Classification := 2]

  return(invisible())
}

#' Parameters for progressive morphological filter
#'
#' The function \link{lasground} with the progressive morphological filter allows for any
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
#' Because the function \link{lasground} applies the morphological operation at the point
#' cloud level the parameter \eqn{c} is set to 1 and cannot be modified.
#' @param b numeric. This is the parameter \eqn{b} in Zhang et al. (2003) (eq. 4 and 5).
#' @param max_ws numeric. Maximum window size to be used in filtering ground returns. This limits
#' the number of windows created.
#' @param dh0 numeric. This is \eqn{dh_0} in Zhang et al. (2003) (eq. 7).
#' @param dhmax numeric. This is \eqn{dh_{max}} in Zhang et al. (2003) (eq. 7).
#' @param s numeric. This is \eqn{s} in Zhang et al. (2003) (eq. 7).
#' @param exp logical. The window size can be increased linearly or exponentially (eq. 4 or 5).
#' @return A list with two components, the windows size sequence and the threshold sequence.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#' @export
util_makeZhangParam = function(b = 2, dh0 = 0.5, dhmax = 3.0, s = 1.0,  max_ws = 20, exp = FALSE)
{
  if (exp & b <= 1)
    stop("b cannot be lower than 1 with an exponentially growing windows", call. = FALSE)

  if (dh0 >= dhmax)
    stop("dh0 greater than dhmax", call. = FALSE)

  if (max_ws < 3)
    stop("Minimum windows size is 3. max_ws cannot must be greater than 3", call. = FALSE)

  if (!is.logical(exp))
    stop("exp should be logical", call. = FALSE)

  if (!exp & b < 1)
    warning("Due to an incoherance in the original paper when b < 1 the sequences of windows size cannot be computed for a linear increasing. The internal routine use the fact that the increses in contant to bypass this issue.", call. = FALSE)


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
      if(exp)
        th = s * (ws - wk[k]) * c + dh0
      else
        th = s*2*b*c+dh0
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
