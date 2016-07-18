#' Leaf area density
#'
#' Computes a leaf area density profile based on the method of Bouvier et al. (see reference)
#'
#' The function assessing the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz]. Then it computes the log
#' of this quantity and divides it by the extinction coefficient k as described in Bouvier
#' et al. By definition the layer 0 will always return 0 because no returns reached
#' the ground. Therefore, the layer 0 is removed from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @param k numeric. is the extinction coefficient
#' @return A data.frame containing the bin elevations (z) and leaf area density for each bin (lad)
#' @examples
#' z = dbeta(seq(0, 0.8, length = 1000), 6, 6)*10
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

#' Gap fraction profile
#'
#' Computes the gap fraction profile using the method of Bouvier et al. (see reference)
#'
#' The function assessing the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz]. By definition the layer 0
#' will always return 0 because no returns reached the ground. Therefore, the layer 0 is removed
#' from the returned results.
#'
#' @param z vector of positive z coordinates
#' @param dz numeric. The thickness of the layers used (height bin)
#' @return A data.frame containing the bin elevations (z) and the gap fraction for each bin (gf)
#' @examples
#' z = dbeta(seq(0, 0.8, length = 1000), 6, 6)*10
#'
#' gapFraction = gapFractionProfile(z)
#'
#' plot(gapFraction, type="l", xlab="Elevation", ylab="Gap fraction")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @seealso \link[lidR:LAD]{LAD}
#' @export gapFractionProfile
gapFractionProfile = function (z, dz = 1)
{
    maxz = max(z)

    if(maxz < 3*dz)
      return(NA_real_)

    bk = seq(0, ceiling(maxz), dz)

    histogram = hist(z, breaks = bk, plot = F)
    height    = histogram$mids
    histogram = histogram$counts

    cs = cumsum(histogram)
    r = (dplyr::lead(cs)-cs)
    r[is.na(r)] = 0
    r = r[-length(r)]

    r = c(length(z)-sum(r), r)

    r = r/cumsum(r)

    r[is.nan(r)] = NA

    z = height[-1]
	  r = r[-1]

    return(data.frame(z, gf = 1-r))
}

