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
