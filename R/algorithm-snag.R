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

#' Snags Segmentation Algorithm
#'
#' This function is made to be used in \link{segment_snags}. It implements an algorithms for snags segmentation
#' based on Wing et al (2015) (see references). This is an automated filtering algorithm that utilizes
#' three dimensional neighborhood lidar point-based intensity and density statistics to remove lidar
#' points associated with live trees and retain lidar points associated with snags.
#'
#' Note that this algorithm strictly performs a classification based on user input while
#' the original publication's methods also included a segmentation step and some pre-
#' (filtering for first and single returns only) and post-process (filtering for only the
#' snag classified points prior to segmentation) tasks which are now expected to be performed
#' by the user. Also, this implementation may have some differences compared with the original
#' method due to potential mis-interpretation of the Wing et al. manuscript, specifically
#' Table 2 where they present four groups of conditional assessments with their required
#' neighborhood point density and average BBPR values (BBPR = branch and bole point ratio;
#' PDR = point density requirement).\cr\cr
#' This algorithm attributes each point in the point cloud (\code{snagCls} column) into the
#' following five snag classes:
#' \itemize{
#' \item 0: live tree - not a snag\cr
#' \item 1: general snag - the broadest range of snag point situations\cr
#' \item 2: small snag - isolated snags with lower point densities\cr
#' \item 3: live crown edge snag - snags located directly adjacent or intermixing with live trees crowns \cr
#' \item 4: high canopy cover snag - snags protruding above the live canopy in dense conditions (e.g.,
#' canopy cover >= 55\%).
#' }
#' The current implementation is known to use a large amount of memory for storing the N x k
#' integer matrix returning the near neighbor indices for each point in the point cloud.
#' Improvements are possible in future package versions.
#'
#' @param neigh_radii numeric. A vector of three radii used in quantifying local-area centered
#' neighborhoods. See Wing et al. (2015) reference page 171 and Figure 4. Defaults are 1.5,
#' 1, and 2 for the sphere, small cylinder and large cylinder neighborhoods, respectively.
#'
#' @param low_int_thrsh numeric. The lower intensity threshold filtering value. See Wing
#' et al. (2015) page 171. Default is 50.
#'
#' @param uppr_int_thrsh numeric. The upper intensity threshold filtering value. See Wing
#' et al. (2015) page 171. Default is 170.
#'
#' @param pt_den_req numeric. Point density requirement based on plot-level point density
#' defined classes. See Wing et al. (2015) page 172. Default is 3.
#'
#' @param BBPRthrsh_mat matrix. A 3x4 matrix providing the four average BBPR (branch and bole
#' point ratio) values for each of the three neighborhoods (sphere, small cylinder and large
#' cylinder) to be used for conditional assessments and classification into the following four snag
#' classes: 1) general snag 2) small snag 3) live crown edge snag 4) high canopy
#' cover snag. See Wing et al. (2015) page 172 and Table 2. This matrix must be provided by
#' the user.
#'
#' @examples
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
#' las <- segment_snags(las, wing2015(neigh_radii = c(1.5, 1, 2), BBPRthrsh_mat = bbpr_thresholds))
#'
#' # Plot it all, tree and snag points...
#' plot(las, color="snagCls", colorPalette = rainbow(5))
#'
#' # Filter and plot snag points only
#' snags <- filter_poi(las, snagCls > 0)
#' plot(snags, color="snagCls", colorPalette = rainbow(5)[-1])
#'
#' # Wing et al's (2015) methods ended with performing tree segmentation on the
#' # classified and filtered point cloud using the watershed method
#'
#' @author
#' Implementation by Andrew Sánchez Meador & Jean-Romain Roussel
#'
#' @references
#' Wing, Brian M.; Ritchie, Martin W.; Boston, Kevin; Cohen, Warren B.; Olsen, Michael J. 2015.
#' Individual snag detection using neighborhood attribute filtered airborne lidar data. Remote
#' Sensing of Environment. 163: 165-179 https://doi.org/10.1016/j.rse.2015.03.013
#'
#' @export
#'
#' @family snags segmentation algorithms
wing2015 = function(neigh_radii = c(1.5,1,2), low_int_thrsh = 50, uppr_int_thrsh = 170, pt_den_req = 3, BBPRthrsh_mat = NULL)
{
  assert_is_numeric(neigh_radii)
  assert_all_are_in_closed_range(neigh_radii, 0, 10)
  assert_is_a_number(low_int_thrsh)
  assert_all_are_in_closed_range(low_int_thrsh, 0, 255)
  assert_is_a_number(uppr_int_thrsh)
  assert_all_are_in_closed_range(uppr_int_thrsh, 0, 255)
  assert_all_are_true(low_int_thrsh < uppr_int_thrsh)
  assert_is_a_number(pt_den_req)
  assert_all_are_in_open_range(pt_den_req, 0, 100)

  if (is.null(BBPRthrsh_mat))
    stop("Branch and bole point ratio thresholds matrix not supplied.")

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTSNG, "wing2015")
    return(C_Wing2015(las, neigh_radii, low_int_thrsh, uppr_int_thrsh, pt_den_req, BBPRthrsh_mat, getThread()))
  }

  class(f) <- c(LIDRALGORITHMSNG, LIDRALGORITHMOPENMP)

  return(f)
}
