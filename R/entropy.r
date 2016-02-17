#' Normalized Shannon diversity index
#'
#' A normalized Shannon vertical complexity index
#'
#' The Shannon diversity index is a measure for quantifying diversity, and is
#' based on the number and frequency of species present. This index, developed by
#' Shannon and Weaver for use in information theory, was successfully transferred
#' to the description of species diversity in biological systems (Shannon 1948).
#' Here it is applied to quantify the diversity and the evenness of a distribution
#' of LiDAR points elevation. It makes bins between 0 and the maximum elevation.
#'
#' @param z vector of positive z coordinates
#' @param by numeric. The thickeness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function vci.
#' @seealso
#' \link[lidR:vci]{vci}
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

	# If z max < 3 it does not have a meaning to compute entropy
	if(zmax < 2)
		return(NA_real_)

  if(min(z) < 0)
    stop("Entropy found negatives values. Returned NA.")

	# Define the x meters bins from 0 to zmax (rounded to the next integer)
	bk = seq(0, ceiling(zmax), by)

	# Compute the p for each bin
	hist = hist(z, breaks = bk, plot=F)$density

	# Remove bin where there are no point because of log(0)
	p = hist[hist > 0]

	# normalized entropy
	S = - sum(p*log2(p)) / length(hist)

	return(S)
}

#' Vertical complexity index
#'
#' A fixed normalization of the entropy function (see references)
#' @param z vector of z coordinates
#' @param by numeric. The thickeness of the layers used (height bin)
#' @param zmax numeric. Used to turn the function entropy to the function vci.
#' @return A number between 0 and 1
#' @return A number between 0 and 1
#' @seealso
#' \link[lidR:entropy]{entropy}
#' @examples
#' z = runif(10000, 0, 10)
#'

#' vci(z, by = 1, zmax = 20)
#'
#' z = abs(rnorm(10000, 10, 1))
#'
#' # expected to be closer to 0.
#' vci(z, by = 1, zmax = 20)
#' @references van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using Lidar-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269. Retrieved from <Go to ISI>://WOS:000288052100009
#' @export vci
vci = function(z, zmax, by = 1)
{
  z = z[z < zmax]

  return(entropy(z, zmax, by))
}

