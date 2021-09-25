#' Eigenvalues-based features at the point level
#'
#' Functions that compute, for each point, the eigenvalues of the covariance matrix of the neighbouring
#' points. The eigenvalues are later used either to segment linear/planar points or to compute derived
#' metrics (see Details).
#'
#' All the functions documented here can be reproduced with \link{point_metrics}. However,
#' \link{point_metrics} is a versatile and multipurpose function that is not as fast as is possible because
#' it calls user-defined R code and that implies computational overheads. These functions are parallelized
#' plain C++ versions of tools users can build with `point_metrics` and are consequently 10-times faster.
#' \describe{
#' \item{**segment_shape**}{The points that meet a given criterion based on the eigenvalue are labelled as
#' approximately coplanar/colinear or any other shape supported.}
#' \item{**point_eigenvalues**}{Computes the eigenvalues of the covariance matrix and computes associated
#' metrics following Lucas et al, 2019 (see references). It is equivalent to `point_metrics(las, .stdshapemetrics)`
#' but much faster}
#' }
#'
#' @return \describe{
#' \item{**segment_shape**}{A LAS object with a new column named after the argument `attribute` that indicates
#' those points that are part of a neighborhood that is approximately of the shape searched (TRUE)
#' or not (FALSE).}
#' \item{**point_eigenvalues**}{A data.frame like \link{point_metrics}}
#' }
#'
#' @template param-las
#' @param algorithm An algorithm for shape detection. lidR has: \link{shp_plane}, \link{shp_hplane}
#' and \link{shp_line}.
#' @param attribute character. The name of the new column to add into the LAS object.
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See the examples.
#'
#' @references Lucas, C., Bouten, W., Koma, Z., Kissling, W. D., & Seijmonsbergen, A. C. (2019).
#' Identification of Linear Vegetation Elements in a Rural Landscape Using LiDAR Point Clouds.
#' Remote Sensing, 11(3), 292.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-keep_random_fraction 0.5")
#'
#' # Use the eigenvalues to estimate if points are part of a local plan
#' las <- segment_shapes(las, shp_plane(k = 15), "Coplanar")
#' #plot(las, color = "Coplanar")
#'
#' # Computes the eigenvalue of each point
#' M <- point_eigenvalues(las, k = 15)
#' M
#'
#' \dontrun{
#'
#' # Drop ground point at runtime
#' las <- segment_shapes(las, shp_plane(k = 15), "Coplanar", filter = ~Classification != 2L)
#' #plot(las, color = "Coplanar")
#' }
#'
#' @export
#' @md
#' @rdname point_eigenvalues
segment_shapes = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  UseMethod("segment_shapes", las)
}

#' @export
segment_shapes.LAS = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  stopif_forbidden_name(attribute)
  assert_is_a_string(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_shp(algorithm)
  lidR.context <- "segment_shapes"

  filter    <- parse_filter(las, filter)
  output    <- algorithm(las, filter)
  las@data[[attribute]] <- output
  return(las)
}

#' @export
#' @rdname point_eigenvalues
#' @param xyz logical. Returns the XYZ coordinates of each points instead of IDs.
#' @param metrics logical. Compute additional metrics such as curvature, linearity, planarity based
#' on the eigenvalues.
#' @param k,r integer and numeric respectively for k-nearest neighbours and radius of the neighborhood
#' sphere. If k is given and r is missing, computes with the knn, if r is given and k is missing
#' computes with a sphere neighbourhood, if k and r are given computes with the knn and a limit on the
#' search distance.
point_eigenvalues = function(las, k, r, xyz = FALSE, metrics = FALSE, filter = NULL)
{
  pointID <- NULL

  if (missing(k) && missing(r))  stop("'k' or 'r' is missing", call. = FALSE)

  # knn + radius
  if (!missing(r) && !missing(k)) {
    assert_is_a_number(k)
    assert_is_a_number(r)
    assert_all_are_positive(k)
    assert_all_are_positive(r)
    k <- as.integer(k)
    mode <- 2L
  }

  # knn
  if (!missing(k) && missing(r)) {
    assert_is_a_number(k)
    assert_all_are_positive(k)
    k <- as.integer(k)
    r <- 0
    mode <- 0L
  }

  # radius
  if (!missing(r) && missing(k)) {
    assert_is_a_number(r)
    assert_all_are_positive(r)
    k <- 0
    mode <- 1L
  }


  assert_is_a_bool(xyz)

  filter <- parse_filter(las, filter)
  M <- C_eigen_metrics(las, k, r, filter, getThreads())
  data.table::setDT(M)
  data.table::setorder(M, pointID)

  if (xyz)
  {
    xyz <- coordinates3D(las)
    data.table::setDT(xyz)
    xyz <- xyz[M[["pointID"]]]
    coln <- names(M)
    coln <- c("X", "Y", "Z", coln)
    M[["X"]] <- xyz$X
    M[["Y"]] <- xyz$Y
    M[["Z"]] <- xyz$Z
    data.table::setcolorder(M, coln)
    M[]
  }

  if (metrics)
  {
    M[["curvature"]]  <- M[["eigen_smallest"]] / (M[["eigen_largest"]] + M[["eigen_medium"]] + M[["eigen_smallest"]])   # Luca et al 2019
    M[["linearity"]]  <- (M[["eigen_largest"]] - M[["eigen_medium"]]) / M[["eigen_largest"]]                            # Luca et al 2019
    M[["planarity"]]  <- (M[["eigen_medium"]] - M[["eigen_smallest"]]) / M[["eigen_largest"]]                           # Luca et al 2019
    M[["sphericity"]] <- M[["eigen_smallest"]] / M[["eigen_largest"]];
    M[["anisotropy"]] <- (M[["eigen_largest"]] - M[["eigen_smallest"]]) / M[["eigen_largest"]]
  }

  return(M)
}
