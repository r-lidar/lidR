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
#' can be done with two methods: \code{"knn_idw"} or \code{"akima"} (see details). The
#' algorithm uses the points classified as "ground" to compute the interpolation. The function
#' forces the original lowest ground point of each pixel (if it exists) to be chosen instead of
#' the interpolated value.
#'
#'Methods:
#'\itemize{
#'\item{\code{knn_idw}: interpolation is done using a k-nearest neighbour approach with
#' an inverse distance weighting (IDW).}
#'\item{\code{akima}: interpolation relies on the \link[akima:interp]{interp} function from
#' package \code{akima}. With this method no extrapolation is done outside of the convex hull
#' determined by the data points. The DTM could contain NA values.}
#'}
#' @param .las LAS objet
#' @param res numeric resolution.
#' @param method character can be \code{"knn_idw"} or \code{"akima"}
#' @param k numeric. number of k-nearest neighbour when selected method is \code{"knn_idw"}
#' @param linear logical indicating whether linear or spline interpolation should be used
#' when selected method is \code{"akima"}
#' @return A RasterLayer from package raster
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' lidar = readLAS(LASfile)
#' plot(lidar)
#'
#' dtm = grid_terrain(lidar)
#' plot3d(dtm)
#' }
#' @seealso
#' \link[lidR:lasnormalize]{lasnormalize}
grid_terrain = function(.las, res = 1, method = "knn_idw", k = 3L, linear = T)
{
  . <- X <- Y <- Z <- NULL

  res %<>% round(1)

  ex = extent(.las)
  xo = seq(floor(ex@xmin),ceiling(ex@xmax), res) %>% round(1)
  yo = seq(floor(ex@ymin),ceiling(ex@ymax), res) %>% round(1)

  grid = expand.grid(X = xo, Y = yo)
  data.table::setDT(grid)

  if(method == "knn_idw")
    Zg = lasterrain(.las, grid, "knn_idw", k = k)
  else if(method == "akima")
    Zg = lasterrain(.las, grid, "akima", linear = linear)
  else
    stop("This method does not exist.")

  grid[, Z := round(Zg, 3)]

  # force grounds point to be dominant
  grid = rbind(grid, grid_metrics(lasfilterground(.las), list(Z = min(Z)), res, start=c(0.5*res,0.5*res)))
  grid = grid[, list(Z = min(Z)), by = .(X,Y)]

  mx = data.table::dcast(grid, X~Y, value.var = "Z")[, X := NULL] %>%  as.matrix
  mx = apply(mx, 1, rev)

  dtm = raster::raster(mx, xmn = min(grid$X)-0.5*res, xmx = max(grid$X)+0.5*res, ymn = min(grid$Y)-0.5*res, ymx = max(grid$Y)+0.5*res)

  return(dtm)
}