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
#' can be done using 3 methods: \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see
#' details). The algorithm uses the points classified as "ground" to compute the interpolation.
#'
#' \describe{
#' \item{\code{knnidw}}{Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse distance weighting (IDW). This is a fast but basic method for spatial
#' data interpolation.}
#' \item{\code{delaunay}}{Interpolation based on Delaunay triangulation using the \link[akima:interp]{interp}
#' function from package \code{akima}. This method is very fast. It makes a linear interpolation
#' within each triangle. Note that with this method no extrapolation is done outside of the
#' convex hull determined by the ground points.}
#' \item{\code{kriging}}{Interpolation is done by universal kriging using the \link[gstat:krige]{krige}
#' function. This method combines the KNN approach with the kriging approach. For each point of interest
#' it kriges the terrain using the k-nearest neighbour ground points. This method is more difficult
#' to manipulate but it is also the most advanced method for interpolating spatial data. }
#' }
#' @param .las LAS objet
#' @param res numeric resolution.
#' @param method character can be \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see details)
#' @param k numeric. number of k-nearest neighbours when the selected method is either \code{"knnidw"} or \code{"kriging"}
#' @param model a variogram model computed with \link[gstat:vgm]{vgm} when the selected method
#' is \code{"kriging"}. If null it performs an ordinary or weighted least squares prediction.
#' @param keep_lowest logical. The function forces the original lowest ground point of each
#' pixel (if it exists) to be chosen instead of the interpolated values.
#' @return A \code{lasmetrics} data.table.
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
#' plot(dtm1)
#' plot(dtm2)
#' plot(dtm3)
#' plot3d(dtm1)
#' plot3d(dtm2)
#' plot3d(dtm3)
#' }
#' @seealso
#' \link[lidR:grid_terrain]{grid_terrain}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[gstat:vgm]{vgm}
#' \link[gstat:krige]{krige}
#' \link[akima:interp]{interp}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[raster:raster]{RasterLayer}
grid_terrain = function(.las, res = 1, method, k = 10L, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  . <- X <- Y <- Z <- NULL

  stopifnotlas(.las)

  ground = suppressWarnings(lasfilterground(.las))

  if(is.null(ground))
    stop("No ground points found. Impossible to compute a DTM.", call. = F)

  ground  = ground@data[, .(X,Y,Z)]

  ext  = extent(.las)
  grid = make_grid(ext@xmin, ext@xmax, ext@ymin, ext@ymax, res)

  Zg = interpolate(ground, grid, method, k, model)

  grid[, Z := round(Zg, 3)]

  # force lowest ground point to be dominant
  if(keep_lowest)
  {
    grid = rbind(grid, grid_metrics(lasfilterground(.las), list(Z = min(Z)), res))
    grid = grid[, list(Z = min(Z)), by = .(X,Y)]
  }

  as.lasmetrics(grid, res)

  return(grid)
}