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
#' The function updates the field \code{Classification}. The points classified as 'ground'
#' get the value 2 according to las specifications (See the ASPRS documentation for the
#' \href{http:#www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#' This function is an implementation of Zhang et al. (2003) algorithm inspired from the
#' implementation made in the PCL library (see references).
#'
#' @param .las a LAS object
#' @param MaxWinSize maximum window size to be used in filtering ground returns (see references)
#' @param slope  slope value to be used in computing the height threshold (see references)
#' @param InitDist initial height above the parameterized ground surface to be considered a ground return (see references)
#' @param MaxDist maximum height above the parameterized ground surface to be considered a ground return (see references)
#' @param CellSize cell size
#' @param base numeric. control the windows sizes
#' @param exponential logical. control the windows sizes
#' @return Nothing. The original LAS object is updated by reference. The 'Classification'
#' column contains 2 for ground according to LAS specifications.
#' @references
#' Zhang, K., Chen, S. C., Whitman, D., Shyu, M. L., Yan, J., & Zhang, C. (2003). A progressive
#' morphological filter for removing nonground measurements from airborne LIDAR data. IEEE
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http:#doi.org/10.1109/TGRS.2003.810682
#' \cr\cr
#' R. B. Rusu and S. Cousins, "3D is here: Point Cloud Library (PCL)," Robotics and Automation
#' (ICRA), 2011 IEEE International Conference on, Shanghai, 2011, pp. 1-4. doi: 10.1109/ICRA.2011.5980567
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, XYZonly = TRUE)
#'
#' lasground(las, MaxWinSize = 40, Slope = 3, MaxDist = 4, InitDist = 0.01, CellSize = 8)
#'
#' plot(las, color = "Classification")
#' }
#' @seealso
#' \link[lidR:LAS-class]{LAS}
#' @importFrom data.table :=
lasground = function(.las, MaxWinSize = 20, Slope = 1.0, InitDist = 0.5, MaxDist = 3.0, CellSize = 1.0, base = 2.0, exponential = TRUE)
{
  cloud = .las@data[, .(X,Y,Z)]
  cloud[, idx := 1:dim(cloud)[1]]

  if("Classification" %in% names(.las@data))
  {
    nground = lidR:::fast_countequal(.las@data$Classification, 2)

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

  idx = MorphologicalFilter(cloud, MaxWinSize, Slope, InitDist, MaxDist, CellSize, base, exponential)

  message(paste(length(idx), "ground points found."))

  .las@data[idx, Classification := 2]

  return(invisible())
}

MorphologicalFilter = function(cloud, MaxWinSize, Slope, InitDist, MaxDist, CellSize, base, exponential)
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

  # Progressively filter ground returns using morphological open
  for (i in 1:length(window_sizes))
  {
    # Create new cloud to hold the filtered results. Apply the morphological
    # opening operation at the current window size.
    cloud_f = MorphologicalOpening(cloud, window_sizes[i])

    # Find indices of the points whose difference between the source and
    # filtered point clouds is less than the current height threshold.
    diff = cloud$Z - cloud_f$Z
    indices = ifelse(diff < height_thresholds[i], TRUE, FALSE)

    # Limit filtering to those points currently considered ground returns
    cloud = cloud[indices]
  }

  return(cloud$idx)
}

MorphologicalOpening = function(cloud_in, resolution)
{
  cloud_temp = data.table::copy(cloud_in)
  cloud_out  = data.table::copy(cloud_in)

  n = dim(cloud_in)[1]
  half_res = resolution / 2

  for (p_idx in 1:n)
  {
    u = cloud_temp[p_idx]

    minx = u$X - half_res
    miny = u$Y - half_res
    maxx = u$X + half_res
    maxy = u$Y + half_res

    pt1_indices = data.table::between(cloud_temp$X, minx, maxx) & data.table::between(cloud_temp$Y, miny, maxy)

    if (sum(pt1_indices) > 0)
    {
      v = cloud_temp[pt1_indices]
      min_pt = min(v$Z)
      cloud_out[p_idx, Z := min_pt]
    }
  }

  cloud_temp = data.table::copy(cloud_out)

  for (p_idx in 1:n)
  {
    u = cloud_temp[p_idx]

    minx = u$X - half_res
    miny = u$Y - half_res
    maxx = u$X + half_res
    maxy = u$Y + half_res

    pt2_indices = data.table::between(cloud_temp$X, minx, maxx) & data.table::between(cloud_temp$Y, miny, maxy)

    if (sum(pt2_indices) > 0)
    {
      v = cloud_temp[pt2_indices]
      max_pt = max(v$Z)
      cloud_out[p_idx, Z := max_pt]
    }
  }

  return(cloud_out)
}

