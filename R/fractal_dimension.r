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



#' fractal_dimension
#'
#' Computes the fractal dimension of a surface. The fractal dimension is a measure
#' of roughness.
#'
#' Fractal dimension compute the roughness based on the box counting method (see Taud and Parrot).
#' If the input has an NA value, it returns NA. If the input is too small it returns NA.
#' If the input matrix is not a square matrix, the function cuts the input matrix to create a square matrix.
#' @param mtx numeric matrix that is the representation of a surface model
#' @return numeric. A number between 0 and 3. 3 being the dimension of a volume
#' @references Taud, H., & Parrot, J.-F. (2005). Mesure de la rugosite des MNT a l'aide de la dimension fractale. Geomorphologie : Relief, Processus, Environnement, 4, 327-338. http://doi.org/10.4000/geomorphologie.622
#' @examples
#' mtx = matrix(runif(100), 10, 10)
#' fractal_dimension(mtx)
#' @export fractal_dimension
fractal_dimension = function(mtx)
{
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

  nbbox = sapply(q, .countBox, mtx=mtx)

  lm = stats::lm(log(nbbox) ~ log(q))

  return(abs(as.numeric(stats::coefficients(lm)[2])))
}

.countBox = function(q, mtx)
{
	  rg <- (row(mtx)-1)%/%q+1
    cg <- (col(mtx)-1)%/%q+1
    rci <- (rg-1)*max(cg) + cg
    N <- prod(dim(mtx))/(q^2)

	  lasclip = lapply(1:N, function(x) mtx[rci==x])
	  box = sapply(lasclip,max)/q

	  return(sum(box))
}