# ===============================================================================
#
# PROGRAMMERS:
#
# andrew.sanchezmeador@nau.edu - https://github.com/bi0m3trics
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel.
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

#' Snag classification
#'
#' Snag classification/segmentation using several possible algorithms (see details).
#' The function attributes a number identifying a snag class (\code{snagCls} attribute) to each point
#' of the point cloud. The classification/segmentation is done at the point cloud level and currently
#' only one algorithm implemented, which uses LiDAR intensity thresholds and specified neighborhoods
#' to differentiate bole and branch from foliage points (see details).
#'
#' @template param-las
#' @param algorithm function. An algorithm for snag segmentation. \code{lidR} has \link{wing2015}.
#' @param attribute character. The returned LAS object automatically has a new
#' attribute (a new column). This parameter is the name of this new attribute.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @export
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzi", filter="-keep_first") # Wing also included -keep_single
#'
#' # For the Wing2015 method, supply a matrix of snag BranchBolePtRatio conditional
#' # assessment thresholds (see Wing et al. 2015, Table 2, pg. 172)
#' bbpr_thresholds <- matrix(c(0.80, 0.80, 0.70,
#'                           0.85, 0.85, 0.60,
#'                           0.80, 0.80, 0.60,
#'                           0.90, 0.90, 0.55),
#'                           nrow =3, ncol = 4)
#'
#' # Run snag classification and assign classes to each point
#' las <- lassnags(las, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = bbpr_thresholds))
#'
#' # Plot it all, tree and snag points...
#' plot(las, color="snagCls", colorPalette = rainbow(5))
#'
#' # Filter and plot snag points only
#' snags <- lasfilter(las, snagCls > 0)
#' plot(snags, color="snagCls", colorPalette = rainbow(5)[-1])
#'
#' # Wing et al's (2015) methods ended with performing tree segmentation on the
#' # classified and filtered point cloud using the watershed method
#' }
lassnags = function(las, algorithm, attribute = "snagCls")
{
  UseMethod("lassnags", las)
}

#' @export
lassnags.LAS = function(las, algorithm, attribute = "snagCls")
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.")

  if (!is(algorithm, "SnagsSegmentation"))
    stop("The algorithm is not an algorithm for snags segmentation.")

  stopif_forbidden_name(attribute)

  lidR.context <- "lassnags"
  snags <- algorithm(las)

  las <- lasaddextrabytes(las, snags, attribute, "Number identifying a snag class")
  return(las)
}

#' @export
lassnags.LAScluster = function(las, algorithm, attribute = "snagCls")
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- lassnags(x, algorithm)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lassnags.LAScatalog = function(las, algorithm, attribute = "snagCls")
{
  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_apply(las, lassnags,  algorithm = algorithm, .options = options)
  output  <- unlist(output)
  ctg     <- catalog(output)

  opt_copy(ctg) <- las
  return(ctg)
}
