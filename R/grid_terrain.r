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



#' Digital Terrain Model
#'
#' Interpolate ground points and create a rasterized digital terrain model. The interpolation
#' is base on \link[lidR:lasterrain]{lasterrain} which provides 3 methods for spatial interpolation:
#' \code{"knnidw"}, \code{"delaunay"} and \code{"kriging"}. The algorithm uses the points classified as "ground"
#' to compute the interpolation. The function forces the original lowest ground point of each
#' pixel (if it exists) to be chosen instead of the interpolated values.
#'
#' @param .las LAS objet
#' @param res numeric resolution.
#' @param ... parameters for \link[lidR:lasterrain]{lasterrain}. Select the method and
#' its parameters for spatial interpolation.
#' @return A RasterLayer from package raster
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' lidar = readLAS(LASfile)
#' plot(lidar)
#'
#' dtm1 = grid_terrain(lidar, method = "knnidw", k = 6)
#' dtm2 = grid_terrain(lidar, method = "delaunay")
#' dtm3 = grid_terrain(lidar, method = "kriging", k = 10)
#'
#' \dontrun{
#' raster::plot(dtm1, col = height.colors(50))
#' raster::plot(dtm2, col = height.colors(50))
#' raster::plot(dtm3, col = height.colors(50))
#' plot3d(dtm1)
#' plot3d(dtm2)
#' plot3d(dtm3)
#' }
#' @seealso
#' \link[lidR:lasterrain]{lasterrain}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[raster:raster]{RasterLayer}
grid_terrain = function(.las, res = 1, ...)
{
  . <- X <- Y <- Z <- NULL

  res %<>% round(1)
  ex = extent(.las)

  grid = make_grid(ex@xmin, ex@xmax, ex@ymin, ex@ymax, res)

  Zg = lasterrain(.las, grid, ...)

  grid[, Z := round(Zg, 3)]

  # force grounds point to be dominant
  grid = rbind(grid, grid_metrics(lasfilterground(.las), list(Z = min(Z)), res, start=c(0.5*res,0.5*res)))
  grid = grid[, list(Z = min(Z)), by = .(X,Y)]

  as.lasmetrics(grid, res)

  return(as.raster(grid))
}