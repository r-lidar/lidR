# ===============================================================================
#
# PROGRAMMERS:
#
# andrew.sanchezmeador@nau.edu - https://github.com/bi0m3trics
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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

#' Snag classification/segmentation of airborne LiDAR point clouds
#'
#' Snag classification/segmentation using with several possible algorithms (see details). The function
#' attributes to each point of the point cloud either a number identifying a snag class (\code{snagCls} column)
#' or a number identifying the detected snag the point comes from (\code{snagID} column). By default the
#' classification/segmentation is done at the point cloud level and there is currently only one algorithm
#' implemented (which uses LiDAR intensity thresholds and specified neighborhoods to differentiate bole
#' and branch from foliage points - see details).
#'
#' @param las An object of the class \code{LAS}
#' @param algorithm character. The name of an algorithm. At present, can only be \code{"wing2015"}.
#' (see sections relevant to each
#' algorithm).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' about the algorithm)
#' @param neigh_radii numeric. A vector of three radii used in quantifying local-area centered neighborhoods. See
#' reference page 171 and Figure 4. Defaults are 1.5, 1, and 2 for the sphere, small cylinder and large cylinder neighborhoods,
#' respectively
#' @param low_int_thrsh numeric. The lower intensity threshold filtering value. See reference page 171. Default is 50
#' @param uppr_int_thrsh numeric. The upper intensity threshold filtering value. See reference page 171. Default is 170
#' @param pt_den_req numeric. Point density requirement based on plot-level point density defined classes. See reference
#' page 172. Default is 3
#' @param bbpr_thresholds matrix. A 3x4 matrix providing the four average BBPR values for each of the three neighborhood
#' (sphere, small cylinder and large cylinder neighborhoods) to be used in for conditional assessments and classification
#' into the following four snag classes: 1) general snag, 2) small snag, 3) live crown edge snag, and 4) high canopy
#' cover snag. See reference page 172 and Table 2. This matrix must be provided by the user.
#' The current implementation is known to use a large amount of memory for storing the N x k integer matrix returning the
#' near neighbor indices for each point in the point cloud. Improvements are possible in future package versions.
#' @return Nothing, the point cloud is updated by reference.
#'
#' @section Wing et al. 2016:
#' This is an automated filtering algorithm that utilizes three dimensional neighborhood lidar point-based
#' intensity and density statistics to remove lidar points associated with live trees and retain lidar points
#' associated with snags developed by Wing et al (2015 - see references).
#' Note that this algorithm strictly performs a classification based on user input while the original publication's
#' methods also included a segmentation step and some pre- (filtering for first and single returns only) and post-
#' process (filtering for only the snag classified points prior to segmentation) tasks which are now expected to
#' be performed by the user. Also, this implementation may have some differences compared with the original method
#' due to potential mis-interpretation of the Wing et al. manuscript, specifically Table 2 where they present four
#' groups of conditional assessments with their required neighborhood point density and average BBPR values (BBPR
#' = branch and bole point ratio; PDR = point density requirement).
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyzi", filter="-drop_z_below 1.37 -first_only -keep_single")
#'
#' # For the Wing2015 method, Supply a matrix of snag BranchBolePtRatio conditional assessment thresholds (see Wing et al
#' # 2015, Table 2, pg. 172)
#' BBPRthrsh_mat <- matrix(c(0.80, 0.80, 0.70,
#'                           0.85, 0.85, 0.60,
#'                           0.80, 0.80, 0.60,
#'                           0.90, 0.90, 0.55),
#'                           nrow =3, ncol = 4)
#'
#' # Run snag classification and assign classes to each point
#' lassnags(las, algorithm = "wing2015", neigh_radii = c(1.5,1,2), low_int_thrsh = 50, uppr_int_thrsh = 170, pt_den_req = 3, bbpr_thresholds = BBPRthrsh_mat)
#'
#' # Plot it all, tree and snag points...
#' plot(las, color="SnagCls", colorPalette = rainbow(5))
#'
#' # Filter and plot snag points only
#' las %<>% lasfilter(SnagCls>0)
#' plot(las, color="SnagCls", colorPalette = rainbow(5)[-1])
#'
#' # To be true to Wing et al (2015), their methods ended with performing tree segmentation on the
#' # classified and filtered point cloud using the watershed method
#' chm = grid_canopy(lasnorm, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 4)
#' chm = as.raster(chm)
#' lastrees(lasnorm, "watershed", chm, th = 4)
#'
#' @author
#' Andrew Sánchez Meador and Jean-Romain Roussel
#'
#' @references
#' Wing, Brian M.; Ritchie, Martin W.; Boston, Kevin; Cohen, Warren B.; Olsen, Michael J. 2015.
#' Individual snag detection using neighborhood attribute filtered airborne lidar data. Remote
#' Sensing of Environment. 163: 165-179 https://doi.org/10.1016/j.rse.2015.03.013\cr\cr
#'
#' @export
#'
lassnags = function (las, algorithm, ..., extra = FALSE)
{
  if (algorithm == "wing2015")
    return(lassnags_wing(las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

# Wing's branch and bole point ratio (BBPR) function, independant of neighborhood
# type (see pg. 172 of Wing et al 2015)
branchBolePtRatio = function(intensity, low_int_thrsh, uppr_int_thrsh)
{
  num = sum(intensity <= low_int_thrsh | intensity >= uppr_int_thrsh)
  denum = length(intensity)
  return(num/denum)
}

#' @export
#' @rdname lassnags
lassnags_wing = function (las, neigh_radii = c(1.5,1,2), low_int_thrsh = 50, uppr_int_thrsh = 170, pt_den_req = 3, bbpr_thresholds = NULL)
{
  if (!is(las, "LAS"))  stop("First argument is not a LAS object")
  if(any(neigh_radii <= 0 | neigh_radii > 3) == TRUE) stop("Incorrect neighborhood radii vector supplied or values are too large!")
  if(low_int_thrsh < 0 | low_int_thrsh >300) stop("Lower intensity threshold incorrect!")
  if(uppr_int_thrsh < 0 | uppr_int_thrsh >300) stop("Upper intensity threshold incorrect!")
  if(low_int_thrsh > uppr_int_thrsh) stop("Intensity thresholds incorrectly specified!")
  if(pt_den_req <= 0 | pt_den_req > 100) stop("Point density ratio incorrectly specified!")
  if(is.null(bbpr_thresholds)) stop("Branch and bole point ratio threshold matirx not supplied!")

  # ====== STEP 0 =======
  # initialization
  verbose("Initializing parameters...")
  pcPtDen = las@header@PHB$`Number of point records`/area(las) # The point cloud point density (per m)
  XYZ = las@data[, .(X,Y,Z)]
  XY  = las@data[, .(X,Y)]
  r1  = neigh_radii[1] # Sphere neighborhood radius
  r2  = neigh_radii[2] # Small cylinder neighborhood radius
  r3  = neigh_radii[3] # Large cylinder neighborhood radius
  n   = nrow(las@data)
  row = 1:n

  sph_BranchBolePtRatio_mean <- sph_BranchBolePtRatio <- sph_PtDen <- sm_cyl_BranchBolePtRatio_mean <- sm_cyl_BranchBolePtRatio <- sm_cyl_PtDen <- lg_cyl_BranchBolePtRatio_mean <- lg_cyl_BranchBolePtRatio <- lg_cyl_PtDen <- numeric(n)

  Int_vec = las@data$Intensity
  Z_vec   = las@data$Z

  # ====== STEP 1 =======
  # Compute the BBPR and point density (PtDen) for the sphere, small cylinder and large cylinder neighborhoods
  # and assign to each point. Then calculate the mean neighborhood BBPR and assign it to each point.

  # ====== STEP 1a =======
  # sphere neighborhood
  k = ceiling(pcPtDen*pi*r1^2/10)*10 # The maximum number of neighbors to be included
  verbose("Computing k-nearest neighbors for sphere neighborhood...")
  nn_idx = RANN::nn2(XYZ, XYZ, k = k, treetype = "kd", searchtype = "radius", radius = r1)$nn.idx

  verbose("Calculating mean neighborhood BBPR...")
  for(i in row)
  {
    idx = nn_idx[i, ]
    sph_BranchBolePtRatio[i] = branchBolePtRatio(Int_vec[idx], low_int_thrsh, uppr_int_thrsh)
    sph_PtDen[i]   = sum(idx > 0)
  }

  for(i in row)
  {
    idx = nn_idx[i, ]
    sph_BranchBolePtRatio_mean[i] = mean(sph_BranchBolePtRatio[idx])
  }

  # ====== STEP 1b =======
  # small cylinder neighborhood
  k = ceiling(pcPtDen*pi*r2^2/10)*10
  verbose("Computing k-nearest neighbors for small cylinder neighborhood...")
  nn_idx = RANN::nn2(XY, XY, k = k, treetype = "kd", searchtype = "radius", radius = r2)$nn.idx

  verbose("Calculating mean neighborhood BBPR...")
  for (i in row)
  {
    idx = nn_idx[i, ]
    sm_cyl_BranchBolePtRatio[i] = branchBolePtRatio(Int_vec[idx[Z_vec[idx]>=Z_vec[i]]], low_int_thrsh, uppr_int_thrsh)
    sm_cyl_PtDen[i] = sum(idx[Z_vec[idx]>=Z_vec[i]] > 0)
  }

  for(i in row)
  {
    idx = nn_idx[i, ]
    sm_cyl_BranchBolePtRatio_mean[i] = mean(sm_cyl_BranchBolePtRatio[idx])
  }

  # ====== STEP 1c =======
  # large cylinder neighborhood
  k = ceiling(pcPtDen*pi*r3^2/10)*10
  verbose("Computing k-nearest neighbors for large cylinder neighborhood...")
  nn_idx = RANN::nn2(XY, XY, k = k, treetype = "kd", searchtype = "radius", radius = r3)$nn.idx

  verbose("Calculating mean neighborhood BBPR...")
  for (i in row)
  {
    idx = nn_idx[i, ]
    lg_cyl_BranchBolePtRatio[i] = branchBolePtRatio(Int_vec[idx], low_int_thrsh, uppr_int_thrsh)
    lg_cyl_PtDen[i] = sum(idx > 0)
  }

  for(i in row)
  {
    idx = nn_idx[i, ]
    lg_cyl_BranchBolePtRatio_mean[i] = mean(lg_cyl_BranchBolePtRatio[idx])
  }

  # ====== STEP 2 =======
  # Point classificaitons based on rough interpretation of Table 2 - pg. 172
  # Vlaues supplied/specified by user in BranchBolePtRatio.mat

  verbose("Classifiying points...")
  ifelse(sph_PtDen>=pt_den_req  & sph_BranchBolePtRatio_mean>=bbpr_thresholds[1,1] &
         sm_cyl_PtDen>=pt_den_req & sm_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[2,1] &
         lg_cyl_PtDen>=pt_den_req & lg_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[3,1],
         las@data[, SnagCls := 1],        # General snag class

  ifelse(sph_PtDen>=2 & sph_PtDen<=pt_den_req & sph_BranchBolePtRatio_mean>=bbpr_thresholds[1,2] &
         sm_cyl_PtDen>=2 & sm_cyl_PtDen<=pt_den_req & sm_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[2,2] &
         lg_cyl_PtDen>=2 & lg_cyl_PtDen<=pt_den_req & lg_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[3,2],
         las@data[, SnagCls := 2],        # Small snag class

  ifelse(sph_PtDen>=pt_den_req & sph_BranchBolePtRatio_mean>=bbpr_thresholds[1,3] &
         sm_cyl_PtDen>=pt_den_req & sm_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[2,3] &
         lg_cyl_PtDen>=pt_den_req*7 & lg_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[3,3],
         las@data[, SnagCls := 3],        # Live crown edge snag class

  ifelse(sph_PtDen>=pt_den_req & sph_BranchBolePtRatio_mean>=bbpr_thresholds[1,4] &
         sm_cyl_PtDen>=pt_den_req & sm_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[2,4] &
         lg_cyl_PtDen>=pt_den_req*15 & lg_cyl_BranchBolePtRatio_mean>=bbpr_thresholds[3,4],
         las@data[, SnagCls := 4],        # High canopy cover snag class

         las@data[, SnagCls := 0]))))     # Remaining points assigned to live tree class
}