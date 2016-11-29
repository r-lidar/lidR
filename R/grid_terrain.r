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
#' Interpol ground points using a k neareast neighbourgh approach and create a rasterized
#' digital terrain model. The interpolation is done by default using an invert distance weighting
#' (IDW). The algorithm uses the points classified as "ground" and for each pixel the
#' algorithm uses the k nearest ground point to compute the interpolation.
#'
#' @param obj LAS objet
#' @param res numeric resolution.
#' @param k numeric. The number of ground points used to interpolate (see
#' \link[lidR:get_ground_elevation]{get_ground_elevation})
#' @param kernel character. Kernel to use. Default is "inv". See \link[kknn:kknn]{kknn}
#' for possible choices.
#' @param ... extra parameter for \link[kknn:kknn]{kknn}
#' @return A RasterLayer from package raster
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' # plot(lidar)
#'
#' dtm = grid_terrain(lidar)
#'
#' # plot3d(dtm)
#' @seealso
#' \link[lidR:normalize]{normalize}
#' @importFrom  data.table := setDT
setGeneric("grid_terrain", function(obj, res = 1, k = 7L, kernel = "inv", ...){standardGeneric("grid_terrain")})

#' @rdname grid_terrain
setMethod("grid_terrain", "LAS",
  function(obj, res = 1, k = 7L, kernel = "inv", ...)
  {
    X <- Y <- Z <- NULL

    ex = extent(obj)
    xo = seq(floor(ex@xmin),ceiling(ex@xmax), res) %>% round(1)
    yo = seq(floor(ex@ymin),ceiling(ex@ymax), res) %>% round(1)

    grid   = expand.grid(X = xo, Y = yo)
    setDT(grid)

    Zg = get_ground_elevation(obj, grid, k, kernel, ...)
    grid[, Z := Zg]

    mx = data.table::dcast(grid, X~Y, value.var = "Z")[, X := NULL] %>%  as.matrix
    mx = apply(mx, 1, rev)

    dtm = raster::raster(mx, xmn = min(grid$X), xmx = max(grid$X), ymn = min(grid$Y), ymx = max(grid$Y))

    return(dtm)
  }
)
