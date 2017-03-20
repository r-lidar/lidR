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

#' Canopy height model based on a triangular irregular network.
#'
#' Interpolation of a triangular irregular network constructed from first returns. This function
#' enables use of the pit-free algorithm developed by Khosravipour et al. (see reference).
#'
#'
#' @param .las A LAS object
#' @param res numeric resolution
#' @param thresholds numeric array. Set of height threholds. If \code{thresholds = 0} the algorithm
#' is a strict rasterizaton of the triangulation of the first returns. However, if an array is passed to
#' the function it becomes the Khosravipour et al. pit-free algorithm.
#' @param max_edge  numeric. Maximum edge-length of a triangle in the Delaunay triangulation
#' used to constrain the pit-free algorithm (see reference).
#' @return An object of class \code{lasmetrics}
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, Classification = FALSE, Intensity = FALSE, filter = "-drop_z_below 0")
#'
#' # Basic triangulation and rasterization
#' chm1 = grid_tincanopy(las, thresholds = 0)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm2 = grid_tincanopy(las, thresholds = c(0,2,5,10,15), max_edge = 1)
#'
#' plot(chm1)
#' plot(chm2)
#' @references Khosravipour, A., Skidmore, A. K., Isenburg, M., Wang, T., & Hussin, Y. A. (2014).
#' Generating pit-free canopy height models from airborne lidar. Photogrammetric Engineering &
#' Remote Sensing, 80(9), 863-872.
grid_tincanopy = function(.las, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = 0.5)
{
  . <- X <- Y <- Z <- ReturnNumber <- NULL
  ex = extent(.las)
  grid = make_grid(ex@xmin, ex@xmax, ex@ymin, ex@ymax, res)
  z = rep(NA, (dim(grid)[1]))

  cloud = .las@data[ReturnNumber == 1, .(X,Y,Z)]

  if(length(thresholds) == 1 & thresholds[1] == 0)
    cat("[Delaunay triangulation of first returns]")
  else if(length(thresholds) > 1)
    cat("[Khosravipour et al. pitfree algorithm]")

  for(th in thresholds)
  {
    if(th == 0)
      edge = 0
    else
      edge = max_edge

    pts = cloud[Z > th]
    Ztemp = interpolate_delaunay(pts, grid, edge)
    z = pmax(z, Ztemp, na.rm = T)
  }

  grid[, Z := z][]
  as.lasmetrics(grid,res)

  return(grid)
}