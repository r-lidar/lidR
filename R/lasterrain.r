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
#' user. The interpolation can be done using 3 methods: \code{"knnidw"},
#' \code{"akima"} or \code{"kriging"} (see details). The algorithm uses the points classified as "ground" to
#' compute the interpolation.
#'
#'\describe{
#'\item{\code{knnidw}}{Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse distance weighting (IDW). This is a very fast but also basic method for spatial
#' data interpolation.}
#'\item{\code{akima}}{Interpolation depends on the \link[akima:interp]{interp} function from
#' package \code{akima}. This method is relatively fast and more advanced than \code{knnidw} and provides
#' good digital terrain models. Notice that with this method no extrapolation is done outside of
#' the convex hull determined by the ground points.}
#' \item{\code{kriging}}{Interpolation is done by universal kriging using \link[gstat:krige]{krige}
#' function. This method is very slow and very difficult to manipulate but it is also the
#' most regognized method to interpolate spatial data.}
#' }
#' @param .las LAS objet
#' @param coord data.frame containing  the coordinates of interest in columns X and Y
#' @param method character can be \code{"knnidw"}, \code{"akima"} or \code{"kriging"} (see details)
#' @param k numeric. number of k nearest neibourgh when selected method is \code{"knnidw"}
#' @param linear logical indicating wether linear or spline interpolation should be used
#' when selected method is \code{"akima"}
#' @param model a variogram model computed with \link[gstat:vgm]{vgm} when selected method is
#' \code{"kriging"}
#' @return Numeric. The predicted elevations.
#' @export
#' @seealso
#' \link[lidR:grid_terrain]{grid_terrain}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[gstat:vgm]{vgm}
#' \link[gstat:krige]{krige}
#' \link[akima:interp]{interp}
lasterrain = function(.las, coord, method, k = 6L, linear = TRUE, model = gstat::vgm(.59, "Sph", 874))
{
  . <- X <- Y <- Z <- NULL

  stopifnotlas(.las)

  if( !"X" %in% names(coord) | !"Y" %in% names(coord))
    stop("Parameter coord does not have a column named X or Y",  call. = F)

  ground = suppressWarnings(lasfilterground(.las))

  if(is.null(ground))
    stop("No ground points found. Impossible to compute a DTM.", call. = F)

  ground = ground@data[, .(X,Y,Z)]

  if(method == "knnidw")
  {
    cat("[using inverse distance weighting]\n")
    return(terrain_knnidw(ground, coord, k))
  }
  else if(method == "akima")
  {
    cat("[using Akima's interpolation]\n")
    return(terrain_akima(ground, coord, linear))
  }
  else if(method == "kriging")
  {
    return(terrain_kriging(ground, coord, model))
  }
  else
    stop("This method does not exist.")
}

terrain_knnidw = function(ground, coord, k = 3L)
{
  . <- X <- Y <- NULL

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

  if (!requireNamespace("akima", quietly = TRUE))
    stop("'akima' package is needed for this function to work. Please install it.", call. = F)

  xo = unique(coord$X) %>% sort()
  yo = unique(coord$Y) %>% sort()

  grid = ground %$% akima::interp(X, Y, Z, xo = xo, yo = yo, linear = linear, duplicate = "user", dupfun = min)

  temp = data.table::data.table(xc = match(coord$X, grid$x), yc = match(coord$Y, grid$y))
  temp[, Zg := grid$z[xc,yc], by = .(xc,yc)]

  return(temp$Zg)
}

terrain_kriging = function(ground, coord, model)
{
  X <- Y <- Z <- NULL

  if (!requireNamespace("gstat", quietly = TRUE))
    stop("'gstat' package is needed for this function to work. Please install it.", call. = F)

  x  = gstat::krige(Z~X+Y, location = ~X+Y, data = ground, newdata = coord, model)
  return(x$var1.pred)
}
