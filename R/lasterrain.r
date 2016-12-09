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



#' Get the elevation of the ground for given coordinates
#'
#' Interpolate ground points and return the elevation at the position of interest given by the
#' parameter. The interpolation can be done using two methods: \code{"knn_idw"} or
#' \code{"akima"} (see details). The algorithm uses the points classified as "ground" to
#' compute the interpolation.
#'
#'Methods:
#'\itemize{
#'\item{\code{knn_idw}: interpolation is done using a k-nearest neighbour approach with
#' an inverse distance weighting (IDW).}
#'\item{\code{akima}: interpolation depends on the \link[akima:interp]{interp} function from
#' package \code{akima}. With this method no extrapolation is done outside of the convex hull
#' determined by the ground points.}
#'}
#' @param .las LAS objet
#' @param coord data.frame containing  the coordinates of interest in columns X and Y
#' @param method character can be \code{"knn_idw"} or \code{"akima"}
#' @param k numeric. number of k nearest neibourgh when selected method is \code{"knn_idw"}
#' @param linear logical indicating wether linear or spline interpolation should be used
#' when selected method is \code{"akima"}
#' @return Numeric. The predicted elevations.
#' @export
#' @seealso
#' \link[kknn:kknn]{kknn}
#' \link[lidR:grid_terrain]{grid_terrain}
lasterrain = function(.las, coord, method = "knn_idw", k = 3L, linear = F)
{
  . <- X <- Y <- Z <- NULL

  stopifnotlas(.las)

  if( !"X" %in% names(coord) | !"Y" %in% names(coord))
    stop("Parameter coord does not have a column named X or Y",  call. = F)

  ground = suppressWarnings(lasfilterground(.las))

  if(is.null(ground))
    stop("No ground points found. Impossible to compute a DTM.", call. = F)

  ground = ground@data[, .(X,Y,Z)]

  if(method == "knn_idw")
    return(terrain_knn_idw(ground, coord, k))
  else if(method == "akima")
    return(terrain_akima(ground, coord, linear))
  else
    stop("This method does not exist.")
}

terrain_knn_idw = function(ground, coord, k = 3L)
{
  X <- Y <- NULL

  nn = RANN::nn2(ground[, .(X,Y)], coord[, .(X,Y)], k = k)
  idx = nn$nn.idx
  w = 1/nn$nn.dist
  w = ifelse(is.infinite(w), 1e8, w)
  z = matrix(ground[as.numeric(idx)]$Z, ncol = dim(w)[2])

  return(rowSums(z*w)/rowSums(w))
}

terrain_akima = function(ground, coord, linear)
{
  . <- X <- Y <- Z <- Zg <- xc <- yc <- NULL

  xo = unique(coord$X) %>% sort()
  yo = unique(coord$Y) %>% sort()

  grid = ground %$% akima::interp(X, Y, Z, xo = xo, yo = yo, linear = linear, duplicate = "user", dupfun = min)

  temp = data.table::data.table(xc = match(coord$X, grid$x), yc = match(coord$Y, grid$y))
  temp[, Zg := grid$z[xc,yc], by = .(xc,yc)]

  return(temp$Zg)
}
