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

#' Gap fraction profile
#'
#' Computes the gap fraction profile using the method of Bouvier et al. (see reference)
#'
#' The function assessing the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz]. By definition the layer 0
#' will always return 0 because no returns pass through the ground. Therefore, the layer 0 is removed
#' from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @return A data.frame containing the bin elevations (z) and the gap fraction for each bin (gf)
#' @examples
#' z = c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z = z[z<45 & z>0]
#'
#' hist(z, n=50)
#'
#' gapFraction = gap_fraction_profile(z)
#'
#' plot(gapFraction, type="l", xlab="Elevation", ylab="Gap fraction")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @seealso \link[lidR:LAD]{LAD}
#' @export gap_fraction_profile
gap_fraction_profile = function (z, dz = 1)
{
  maxz = max(z)

  bk = seq(0, ceiling(maxz), dz)

  histogram = graphics::hist(z, breaks = bk, plot = F)
  h = histogram$mids
  p = histogram$counts/sum(histogram$count)

  p = c(p, 0)

  cs = cumsum(p)
  i = data.table::shift(cs) /cs
  i[is.na(i)] = 0

  i[is.nan(i)] = NA

  z = h[-1]
  i = i[-c(1, length(i))]

  return(data.frame(z, gf = i))
}

#' Leaf area density
#'
#' Computes a leaf area density profile based on the method of Bouvier et al. (see reference)
#'
#' The function assessing the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz] (see \link[lidR:gap_fraction_profile]{gap_fraction_profile}).
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
#' @seealso \link[lidR:gap_fraction_profile]{gap_fraction_profile}
#' @export LAD
LAD = function(z, dz = 1, k = 0.5) # (Bouvier et al. 2015)
{
	ld = gap_fraction_profile(z, dz)

	if(anyNA(ld))
	  return(NA_real_)

	lad = ld$gf
	lad = -log(lad)/k

	lad[is.infinite(lad)] = NA

	lad = lad

	return(data.frame(z = ld$z, lad))
}

#' Normalized Shannon diversity index
#'
#' A normalized Shannon vertical complexity index
#'
#' The Shannon diversity index is a measure for quantifying diversity and is
#' based on the number and frequency of species present. This index, developed by
#' Shannon and Weaver for use in information theory, was successfully transferred
#' to the description of species diversity in biological systems (Shannon 1948).
#' Here it is applied to quantify the diversity and the evenness of an elevational distribution
#' of LiDAR points. It makes bins between 0 and the maximum elevation.
#'
#' @param z vector of positive z coordinates
#' @param by numeric. The thickeness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function \link[lidR:VCI]{VCI}.
#' @seealso
#' \link[lidR:VCI]{VCI}
#' @examples
#' z = runif(10000, 0, 10)
#'
#' # expected to be close to 1. The highest diversity is given for a uniform distribution
#' entropy(z, by = 1)
#'
#'  z = runif(10000, 9, 10)
#'
#' # Must be 0. The lowest diversity is given for a unique possibility
#' entropy(z, by = 1)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be between 0 and 1.
#' entropy(z, by = 1)
#' @references
#' Pretzsch, H. (2008). Description and Analysis of Stand Structures. Springer Berlin Heidelberg. http://doi.org/10.1007/978-3-540-88307-4 (pages 279-280)
#' Shannon, Claude E. (1948), "A mathematical theory of communication," Bell System Tech. Journal 27, 379-423, 623-656.
#' @return A number between 0 and 1
#' @export entropy
entropy = function(z, by = 1, zmax = NULL)
{
  # Fixed entropy (van Ewijk et al. (2011)) or flexible entropy
  if(is.null(zmax))
	  zmax = max(z)

	# If zmax < 3 it is meaningless to compute entropy
	if(zmax < 2)
		return(NA_real_)

  if(min(z) < 0)
    stop("Entropy found negatives values. Returned NA.")

	# Define the x meters bins from 0 to zmax (rounded to the next integer)
	bk = seq(0, ceiling(zmax), by)

	# Compute the p for each bin
	hist = hist(z, breaks = bk, plot=F)$count
	hist = hist/sum(hist)

	# Remove bin where there are no points because of log(0)
	p    = hist[hist > 0]
	pref = rep(1/length(hist), length(hist))

	# normalized entropy
	S = - sum(p*log(p)) / -sum(pref*log(pref))

	return(S)
}

#' Vertical Complexity Index
#'
#' A fixed normalization of the entropy function (see references)
#' @param z vector of z coordinates
#' @param by numeric. The thickness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function vci.
#' @return A number between 0 and 1
#' @seealso
#' \link[lidR:entropy]{entropy}
#' @examples
#' z = runif(10000, 0, 10)
#'

#' VCI(z, by = 1, zmax = 20)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be closer to 0.
#' VCI(z, by = 1, zmax = 20)
#' @references van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using LAS-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269. Retrieved from <Go to ISI>://WOS:000288052100009
#' @export VCI
VCI = function(z, zmax, by = 1)
{
  z = z[z < zmax]

  return(entropy(z, by, zmax))
}