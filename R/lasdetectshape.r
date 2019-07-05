#' Estimation of the shape of the points neighborhood
#'
#' Computes the eigenvalues of the covariance matrix of the neighbouring points using several possible
#' algorithms. The points that meet a given criterion based on the eigenvalue are labeled as approximately
#' coplanar/colinear or any other shape supported.
#'
#' @return A LAS object with a new column named after the argument \code{attribute} that indicates
#' those points that are part of a neighborhood that is approximately of the shape searched (TRUE)
#' or not (FALSE).
#'
#' @param las an object of class LAS
#' @param algorithm An algorithm for shape detection. lidR has: \link{shp_plane}, \link{shp_hplane}
#' and \link{shp_line}.
#' @param attribute character. The name of the new column to add into the LAS object.
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See also examples.
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' las <- lasdetectshape(las, shp_plane(k = 15), "Coplanar")
#' plot(las, color = "Coplanar")
#'
#' # Drop ground point at runtime
#' las <- lasdetectshape(las, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)
#' plot(las, color = "Coplanar")
#' }
#'
#' @export
lasdetectshape = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  UseMethod("lasdetectshape", las)
}

#' @export
lasdetectshape.LAS = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  stopif_forbidden_name(attribute)
  assert_is_a_string(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_shp(algorithm)
  lidR.context <- "lasdetectshape"

  filter    <- parse_filter(las, filter)
  output    <- algorithm(las, filter)
  las@data[[attribute]] <- output
  return(las)
}

parse_filter = function(las, filter, k)
{
  if (!is.null(filter))
    return(lasfilter_(las, list(filter)))

  return(TRUE)
}
