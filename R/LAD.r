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



#' Leaf area density
#'
#' Computes a leaf area density profile based on the method of Bouvier et al. (see reference)
#'
#' The function assessing the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz] (see \link[lidR:gapFractionProfile]{gapFractionProfile}).
#' Then it computes the log of this quantity and divides it by the extinction coefficient k as described in Bouvier
#' et al. By definition the layer 0 will always return the infinity because no returns pass through
#' the ground. Therefore, the layer 0 is removed from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @param k numeric. is the extinction coefficient
#' @return A data.frame containing the bin elevations (z) and leaf area density for each bin (lad)
#' @examples
#' z = c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z = z[z<45 & z>0]
#'
#' lad = LAD(z)
#'
#' plot(lad, type="l", xlab="Elevation", ylab="Leaf area density")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @seealso \link[lidR:gapFractionProfile]{gapFractionProfile}
#' @export LAD
LAD = function(z, dz = 1, k = 0.5) # (Bouvier et al. 2015)
{
	ld = gapFractionProfile(z, dz)

	if(anyNA(ld))
	  return(NA_real_)

	lad = ld$gf
	lad = -log(lad)/k

	lad[is.infinite(lad)] = NA

	lad = lad

	return(data.frame(z = ld$z, lad))
}

