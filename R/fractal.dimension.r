#' fractal.dimension
#'
#' Computes the fractal dimention of a surface. The fractal dimension is a measure
#' of roughness.
#'
#' fractal dimension compute the roughness based on box counting method  (see Taud and Parrot)
#' @param mtx a matrix which is the representation of a surface model
#' @return A number between 0 and 3. 3 Being the dimension of a volume
#' @references Taud, H., & Parrot, J.-F. (2005). Mesure de la rugosite des MNT a l'aide de la dimension fractale. Geomorphologie : Relief, Processus, Environnement, 4, 327-338. http://doi.org/10.4000/geomorphologie.622
#' @export fractal.dimension
#' @importFrom RcppArmadillo fastLmPure
fractal.dimension = function(mtx)
{
	if( sum(is.na(mtx)) > 0 )
		return(as.numeric(NA))

	size = min(dim(mtx))

	if( size < 6)
		return(as.numeric(NA))

	size = ifelse(size %% 2 == 0, size, size-1)

    mtx = mtx[1:size, 1:size]

	q = 1:size
	q = q[size %% q == 0]

	if(length(q) < 3)
		return(as.numeric(NA))

	nbbox = sapply(q, countBox, mtx=mtx)

	lm = RcppArmadillo::fastLmPure(cbind(1,log(q)), log(nbbox))

	return(abs(as.numeric(coefficients(lm)[2])))
}

countBox = function(q, mtx)
{
	  rg <- (row(mtx)-1)%/%q+1
    cg <- (col(mtx)-1)%/%q+1
    rci <- (rg-1)*max(cg) + cg
    N <- prod(dim(mtx))/(q^2)

	  clip = lapply(1:N, function(x) mtx[rci==x])
	  box = sapply(clip,max)/q

	  return(sum(box))
}