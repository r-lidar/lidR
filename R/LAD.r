#' Leaf area density
#'
#' Computes a leaf area density profile based on Bouvier et al. method (see reference)
#'
#' @param z vector of positive z coordinates
#' @param by numeric. The thickeness of the layers used (height bin)
#' @param k numeric. is the extinction coefficient
#' @return A data.frame containing the bins elevations (z) and leaf area density for each bin (lad)
#' @examples
#' z = dbeta(seq(0, 0.8, length = 1000), 6, 6)*10
#'
#' lad = LAD(z)
#'
#' plot(1:length(lad)~lad, type="l", ylab="Elevation", xlab="Leaf area density")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @export LAD
LAD = function(z, by = 1, k = 0.5) # (Bouvier et al. 2015)
{
	ld = .leafdensity(z, by)

	lad = 1-ld$ld
	lad = -log(lad)/k

	lad[is.infinite(lad)] = NA

	z = ld$z[-1]
	lad = lad[-1]

	return(data.frame(z, lad))
}

.leafdensity = function (z, by = 1)
{
    maxz = max(z)

    bk = seq(0, ceiling(maxz), by)

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

    return(data.frame(z = height, ld = r))
}

