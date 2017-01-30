# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================


#' Classify points as ground or not ground
#'
#' Implements the Progressive Morphological Filter for segmentation of ground points.
#' The function updates the field \code{Classification}. The points classified as 'ground'
#' get the value 2 according to las specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#' This function is a wrapper for the Progressive Morphological Filter implemented in the PCL
#' library (Point Cloud Library, see reference).
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
#' Transactions on Geoscience and Remote Sensing, 41(4 PART I), 872–882. http://doi.org/10.1109/TGRS.2003.810682
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
  ex = extent(.las)
  npt = dim(.las@data)[1]
  .las@data[, id := 1:npt]

  f = function(Z, N) {
    id = which.min(Z)
    return(list(Z = Z[id], N = N[id]))
  }

  A = grid_metrics(.las, f(Z,id), CellSize)
  A[, flag := 0]

  k = 1:100
  if(!exponential)
    wk = 2*k*base + 1
  else
    wk = 2*base^k + 1

  wk = wk[wk < MaxWinSize]

  n = length(wk)
  dtk = numeric(n)
  dtk[1] = InitDist
  dtk[2:n] = Slope * (wk[2:n] - wk[1:(n-1)]) * CellSize + InitDist
  dtk[dtk > MaxDist] = MaxDist

  for(i in 1:n)
  {
    w = wk[i]
    dt = dtk[i]

    for(j in 1:dim(A)[i])
    {
      id = RANN::nn2(A[, .(X,Y,Z)], A[i, .(X,Y,Z)], searchtype = "radius", k = 100, radius = w/2)$nn.idx
      id = id[id > 0]

      zf = min(A[id]$Z)

      A[id, Zf := zf]
    }
  }

  idground = A[flag == 0 & N > 0]$N

  .las@data[, Classification := 1]
  .las@data[idground, Classification := 2]

  return(invisible(NULL))
}

erosion = function(Z, w)
{
  w = w - 1
  n = length(Z)
  Zf = numeric(n)

  for(i in 1:n)
  {
    pos = (i-w/2):(i+w/2)
    pos = pos[pos > 0 & pos <= n ]
    Zf[i] = min(Z[pos])
  }

  return(Zf)
}

dilatation = function(Z, w)
{
  w = w - 1
  n = length(Z)
  Zf = numeric(n)

  for(i in 1:n)
  {
    pos = (i-w/2):(i+w/2)
    pos = pos[pos > 0 & pos <= n ]
    Zf[i] = max(Z[pos])
  }

  return(Zf)
}

opening = function(Z, w)
{
  dilatation(erosion(Z, w), w)
}