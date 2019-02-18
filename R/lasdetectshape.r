#' Estimation of the shape of the neighborhood of points
#'
#' For each point, computes the eigenvalues of the covariance matrix of the neighbouring points. The
#' points that meet a given criterion based on the eigenvalue are labeled as approximately coplanar/colinear
#' or any other shape supported by the function (see details).
#'
#' In the following \eqn{a1, a2, a3} denote the eigenvalues of the covariance matrix of the neighbouring
#' points in ascending order. \eqn{th1, th2, th3} denote a set of threshold values. Points are labelled
#' \code{TRUE} if they meet the following criteria. \code{FALSE} otherwise.\cr
#' \describe{
#' \item{plane}{Detection of plans based on criteria defined by Limberger & Oliveira (2015) (see references).
#' A points is labelled as approximately coplanar if: \deqn{a2 > (th1*a1) and (th2*a2) > a3} It expects
#' two threshold values. \code{th = c(25,6) is a good start.}}
#' \item{h-plane}{The same than 'plane' but with an extra test on the orientation of the Z vector
#' of the principal components to test the horizontality of the surface.  \deqn{a2 > (th1*a1) and (th2*a2) > a3 and |Z| > th3 }
#' |Z| should be exactly equal to 1 in theory. In practice 0.98 or 0.99 should be good. \code{th = c(25,6,0.98) is a good start.}}
#' \item{line}{Detection of lines inspired by Limberger & Oliveira (2015) criterion. A points is
#' labelled as approximately colinear if: \deqn{th1*a2 < a3 and th1*a1 < a3}
#' It expect one threshold value. \code{th = 10} is a good start.}
#' }
#'
#' @return A LAS object with a new column names \code{Coplanar}, \code{Hcoplanar} or \code{Colinear} that indicates
#' those points that are part of a neighborhood that is approximately coplanar/colinear (TRUE) or not
#' (FALSE).
#'
#' @param las an object of class LAS
#' @param k interger. The number of k-nearest neighbors.
#' @param shape character. One of "plane", "h-plane","line",
#' "colinear2". See details.
#' @param th numeric. The thresholds values used in labelling criterion (see details). User must
#' provide a vector with the good number of values depending on the method used.
#' @param attribute character. The name of the new column to add into the LAS object. If missing a
#' default name is used.
#' @param filter formula of logical predicates. Enable to run the function only on points of interest
#' in an optimized way. See also examples.
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' las = lasdetectshape(las, "plane", th = c(25,6), k = 15)
#' plot(las, color = "Coplanar")
#'
#' # Drop ground point at runtime for example
#' las = lasdetectshape(las, "plane", th = c(25,6), k = 15, filter = ~Classification != 2)
#' plot(las, color = "Coplanar")
#' }
#'
#' @references
#' Limberger, F. A., & Oliveira, M. M. (2015). Real-time detection of planar regions in unorganized
#' point clouds. Pattern Recognition, 48(6), 2043–2053. https://doi.org/10.1016/j.patcog.2014.12.020\cr\cr
#' @export
lasdetectshape = function(las, shape, th, attribute, k = 8, filter = NULL)
{
  UseMethod("lasdetectshape", las)
}

#' @export
lasdetectshape.LAS = function(las, shape, th, attribute, k = 8, filter = NULL)
{
  shapes    <- c("plane", "h-plane", "line")
  shape     <- match.arg(shape, shapes)
  method    <- which(shape == shapes)
  attribute <- if (missing(attribute)) c("Coplanar", "Hcoplanar", "Colinear")[method]

  if      (shape == "plane"   & length(th) != 2) stop("Method 'plane' expects 2 threshold values", call. = FALSE)
  else if (shape == "h-plane" & length(th) != 3) stop("Method 'h-plane' expects 3 threshold values", call. = FALSE)
  else if (shape == "line"    & length(th) != 1) stop("Method 'line' expects 1 threshold values", call. = FALSE)
  stopifnot(nrow(las@data) > k)

  filter    <- parse_filter(las, filter, k)
  output    <- C_lasdetectshape(las, method, th, k, filter, getThread())
  las@data[[attribute]] <- output
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
