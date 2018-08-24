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

#' Classify points as ground based on Progressive Morphological Filter
#'
#' Implements algorithms for segmentation of ground points. The function updates the field
#' \code{Classification} of the LAS input object. The points classified as 'ground' are
#' assigned a value of 2 according to las specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#' This method is an implementation of the Zhang et al. (2003) algorithm (see reference).
#' Note that this is not a strict implementation of Zhang et al. This algorithm works at the point
#' cloud level without any rasterization process. The morphological operator is applied on
#' the point cloud, not on a raster. Also, Zhang et al. proposed some formulas (eq. 4, 5 and 7)
#' to compute the sequence of windows sizes and thresholds. Here, these parameters are free
#' and specified by the user. The function \link{util_makeZhangParam} enables computation
#' of the parameters according to the original paper.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param ws numeric. Sequence of windows sizes to be used in filtering ground returns.
#' The values must be positive and in the same units as the point cloud (usually meters, occasionally
#' feet).
#' @param th numeric. Sequence of threshold heights above the parameterized ground surface to be
#' considered a ground return. The values must be positive and in the same units as the point cloud.
#' @param last_returns logical. The algorithm will use only the last returns (including the first returns
#' in the cases of single return) to run the algorithm. If FALSE all the returns are used. If the fields
#' \code{'ReturnNumber'} or \code{'NumberOfReturns'} are not specified \code{'last_returns'} is turned
#' to \code{FALSE} automatically.
#'
#' @template return-lasground
#'
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682.
#'
#' @export
#' @family lasground
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyzRN")
#'
#' ws = seq(3,12, 3)
#' th = seq(0.1, 1.5, length.out = length(ws))
#'
#' lasground(las, "pmf", ws, th)
#'
#' plot(las, color = "Classification")
lasground_pmf = function(las, ws, th, last_returns = TRUE)
{
  UseMethod("lasground_pmf", las)
}

#' @export
lasground_pmf.LAS = function(las, ws, th, last_returns = TRUE)
{
  assertive::assert_is_numeric(ws)
  assertive::assert_is_numeric(th)
  assertive::assert_all_are_positive(ws)
  assertive::assert_all_are_positive(th)
  assertive::assert_are_same_length(ws, th)

  return(lasground_generic(las, method = "pmf", last_returns, ws = ws, th = th))
}

#' @export
lasground_pmf.LAScluster = function(las, ws, th, last_returns = TRUE)
{
  x <- readLAS(las)
  if (is.null(x)) return(NULL)
  lasground_pmf(x, ws, th, last_returns)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasground_pmf.LAScatalog = function(las, ws, th, last_returns = TRUE)
{
  las@input_options$select <- "*"

  output      <- catalog_apply2(las, lasground_pmf, ws = ws, th = th, last_returns = last_returns,  need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}