#' Leaf area density
#'
#' Computes a leaf area density profile based on Bouvier et al. method (see reference)
#'
#' @param z vector of positive z coordinates
#' @param by numeric. The thickeness of the layers used (height bin)
#' @param threshold numeric. Remove points below the threshold
#' @return numeric vector containing the leaf area density for each bin
#' @examples
#' z = dbeta(seq(0, 0.8, length = 1000), 6, 6)*10
#'
#' lad = LAD(z)
#'
#' plot(1:length(lad)~lad, type="l", ylab="Elevation", xlab="Leaf area density")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive models of forest inventory attributes using an area-based approach with airborne LiDAR data. Remote Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @export LAD
LAD = function(z, by=1, threshold = 0) # (Bouvier et al. 2015)
{
	maxz = max(z)
	z = z[z > threshold]

	if(maxz < 3)
		return(NA_real_)

	bk = seq(0, ceiling(maxz), 1)
	histogram = hist(z, breaks = bk, plot=F)$count

	cs = cumsum(histogram)
	lad = cs/dplyr::lead(cs)

	lad[is.nan(lad)] = 0
	lad[is.infinite(lad)] = 1

	lad = lad[1:(length(lad)-1)]
	lad = -2*log(lad)

	return(lad)
}
