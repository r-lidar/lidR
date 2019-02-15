#' Test the shape of the neighborhood
#'
#' An approximate coplanarity and colinearity tests. Coplanarity test is based on Limberger & Oliveira
#' (2015). For each point it looks for the k-nearest neighbors. It computes the eigenvalues of the
#' covariance matrix. The points that meet the following criteria are labeled as approximately coplanar:\cr
#' \deqn{a_2 > (th_1*a_1) and (th_2*a_2) > a_3}
#' \eqn{a_1, a_2, a_3} being the eigenvalues of the neighborhood (defined by k-nearest neighbors) in
#' ascending order.\cr\cr
#' Colinearity test performs the same computation but change the critera to label point as approximatly
#' colinear:
#' \deqn{th_1*a_2 < a_3 and (th_1*a_1) < a_3}
#'
#' @return A LAS object with a new column names \code{Coplanar} or \code{Colinear} that indicates
#' those points that are part of a neighborhood that is approximately coplanar/colinear (TRUE) or not
#' (FALSE).
#'
#' @param las an object of class LAS
#' @param k interger. The number of k-nearest neighbors.
#' @param th1 numeric. The threshold 1 used in labelling criteria
#' @param th2 numeric. The threshold 2 used in labelling criteria
#' @param filter formula of logical predicates. Enable to run the function only on points of interest
#' in an optimized way. See also examples.
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' las = lascoplanar(las, k = 20)
#' plot(las, color = "Coplanar")
#' }
#'
#' @references
#' Limberger, F. A., & Oliveira, M. M. (2015). Real-time detection of planar regions in unorganized
#' point clouds. Pattern Recognition, 48(6), 2043–2053. https://doi.org/10.1016/j.patcog.2014.12.020
#' @export
#' @name princomp
lascoplanar = function(las, k = 8, th1 = 25, th2 = 6, filter = NULL)
{
  stopifnot(nrow(las@data) > k, th1 > 0, th2 > 0)
  filter <- parse_filter(las, filter, k)
  out <- C_lascoplanar(las, k, th1, th2, filter, getThread())
  las@data[["Coplanar"]] <- out
  return(las)
}

#' @export
#' @rdname princomp
lascolinear = function(las, k = 8, th1 = 10, filter = NULL)
{
  stopifnot(nrow(las@data) > k, th1)
  filter <- parse_filter(las, filter, k)
  out <- C_lascoplanar(las, k, th1, 0, filter, getThread())
  las@data[["Colinear"]] <- out
  return(las)
}

parse_filter = function(las, filter, k)
{
  if (!is.null(filter))
  {
    filter <- lasfilter_(las, list(filter))
    if (sum(filter) < k) stop("No enought point found with the predicate filter", call. = FALSE)
  }
  else
  {
    filter <- TRUE
  }

  return(filter)
}
