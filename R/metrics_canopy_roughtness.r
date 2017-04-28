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

# fractal_dimension
#
# Computes the fractal dimension of a surface. The fractal dimension is a measure
# of roughness.
#
# Fractal dimension computes the roughness based on the box counting method (see Taud and Parrot).
# If the input has an NA value, it returns NA. If the input is too small it returns NA.
# If the input matrix is not a square matrix, the function cuts the input matrix to create a square matrix.
# @param mtx numeric matrix that is the representation of a surface model
# @return numeric. A number between 0 and 3. 3 being the dimension of a volume
# @references Taud, H., & Parrot, J.-F. (2005). Mesure de la rugosite des MNT a l'aide de la dimension fractale. Geomorphologie : Relief, Processus, Environnement, 4, 327-338. http://doi.org/10.4000/geomorphologie.622
# @examples
# mtx = matrix(runif(100), 10, 10)
# fractal_dimension(mtx)
# @export fractal_dimension
fractal_dimension = function(x, plot = FALSE)
{
  if (is(x, "RasterLayer"))
    x = raster::as.matrix(x)
  else if(!is.matrix(x))
    stop("fractal_dimension: mtx should be a RasterLayer or a Matrix", call. = FALSE)

  mtx = x

  if( sum(is.na(mtx)) > 0 )
    return(NA_real_)

  size = min(dim(mtx))

  if( size < 6)
    return(NA_real_)

  size = ifelse(size %% 2 == 0, size, size-1)

  mtx = mtx[1:size, 1:size]

  q = 1:size
  q = q[size %% q == 0]

  if(length(q) < 3)
    return(as.numeric(NA))

  nbbox = sapply(q, countBox, mtx = mtx)

  lm = stats::lm(log(nbbox) ~ log(q))

  if (plot)
  {
    plot(log(nbbox) ~ log(q))
    abline(lm)
  }

  return(abs(as.numeric(stats::coefficients(lm)[2])))
}

countBox = function(q, mtx)
{
	  rg  <- (row(mtx)-1) %/% q + 1
    cg  <- (col(mtx)-1) %/% q + 1
    rci <- (rg-1) * max(cg) + cg
    N   <- prod(dim(mtx))/(q^2)

	  clip = lapply(1:N, function(x) mtx[rci == x])
	  box = sapply(clip, max)/q

	  return(sum(box))
}


#' Rumple index roughness
#'
#' Computes the roughness of a surface represented by a matrix as the ratio between visible area
#' over the projected area.
#'
#' @param x A matrix or a RasterLayer. The surface.
#' @param res numeric the resolution of the surface
#'
#' @return numeric.
#'
#' @export
#' @examples
#' # Perfectly flat surface, rumple_index = 1
#' surface = matrix(rep(2, 100), 10)
#' rumple_index(surface, 1)
#'
#' # Rought surface, rumple_index > 1
#' surface = matrix(runif(100), 10)
#' rumple_index(surface, 1)
#'
#' # This measure of roughness is scale dependent
#' rumple_index(surface, 1)
#' rumple_index(surface, 2)
rumple_index = function(x, res)
{
  if (is(x, "RasterLayer"))
    x = raster::as.matrix(x)
  else if(!is.matrix(x))
    stop("x should be a RasterLayer or a Matrix")

  x = cbind(0, x)
  x = rbind(0, x)

  r = raster::raster(x)

  kernel = matrix(rep(0,9), ncol = 3)
  kernel[2,2] = 2
  kernel[2,3] = -1
  kernel[3,2] = -1

  y = focal(r, w = kernel, fun = sum, na.rm = TRUE)
  y = as.matrix(y)
  y = y[,-1]
  y = y[-1,]
  y[, dim(y)[2]] = 0
  y[dim(y)[1], ] = 0
  y[is.na(x)] = NA
  y = abs(y)

  z = res^2 + res*y

  return( sum(z, na.rm = T)/(sum(!is.na(z))*res^2) )
}

# rumple_index = function(x,y,z)
# {
#   if(length(x) != length(y) | length(x) != length(z))
#     stop("Different lengths for x,y,z", call. = FALSE)
#
#   keep = !(is.na(x) | is.na(y) | is.na(z))
#   x = x[keep]
#   y = y[keep]
#   z = z[keep]
#
#   X = cbind(x,y,z)
#   dn = suppressMessages(geometry::delaunayn(X[,1:2]))
#   N = triangle_information(dn, X)
#
#   area  = sum(N[,5])
#   parea = sum(N[,6])
#   rumple = area/parea
#
#   return(rumple)
# }