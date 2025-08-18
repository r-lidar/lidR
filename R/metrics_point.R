#' Point-based metrics
#'
#' Computes a series of user-defined descriptive statistics for a LiDAR dataset for each point based
#' on its k-nearest neighbours or its sphere neighbourhood.
#'
#' When the neighbourhood is knn the user-defined function is fed with the current
#' processed point and its k-1 neighbours. The current point being considered as
#' the 1-neighbour with a distance 0 to the reference point. The points are ordered
#' by distance to the central point. When the neighbourhood is a sphere the processed
#' point is also included in the query but points are coming in a random order. `point_eigenmetrics`
#' computes the eigenvalues of the covariance matrix and computes associated metrics following
#' Lucas et al, 2019 (see references). It is equivalent to `point_metrics(las, .stdshapemetrics)`
#' but much faster because it is optimized and parallelized internally.
#'
#' @section Performances:
#' It is important to bear in mind that this function is very fast for the feature it provides i.e.
#' mapping a user-defined function at the point level using optimized memory management. However, it
#' is still computationally demanding.\cr\cr
#' To help users to get an idea of how computationally demanding this function is, let's compare it to
#' \link{pixel_metrics}. Assuming we want to apply `mean(Z)` on a 1 km² tile with 1 point/m²
#' with a resolution of 20 m (400 m² cells), then the function `mean` is called roughly 2500
#' times (once  per cell). On the contrary, with `point_metrics`, `mean` is called 1000000
#' times (once per point). So the function is expected to be more than 400 times slower in this specific
#' case (but it does not provide the same feature).\cr\cr
#' This is why the user-defined function is expected to be well-optimized, otherwise it might drastically
#' slow down this already heavy computation. See examples.
#'
#' @param las An object of class LAS
#' @param func formula. An expression to be applied to each point neighbourhood (see also
#' \link{template_metrics}).
#' @param k,r integer and numeric respectively for k-nearest neighbours and radius of the neighborhood
#' sphere. If k is given and r is missing, computes with the knn, if r is given and k is missing
#' computes with a sphere neighborhood, if k and r are given computes with the knn and a limit on the
#' search distance.
#' @param xyz logical. Coordinates of each point are returned in addition to each metric. Otherwise an
#' ID referring to each point.
#' @param coeffs logical. Principal component coefficients are returned
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See examples.
#' @param ... unused.
#' @param metrics logical. Compute metrics or not
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' # Read only 0.5 points/m^2 for the purposes of this example
#' las = readLAS(LASfile, filter = "-thin_with_grid 2")
#'
#' # Computes the eigenvalues of the covariance matrix of the neighbouring
#' # points and applies a test on these values. This function simulates the
#' # 'shp_plane()' algorithm from 'segment_shape()'
#' plane_metrics1 = function(x,y,z, th1 = 25, th2 = 6) {
#'   xyz <- cbind(x,y,z)
#'   cov_m <- cov(xyz)
#'   eigen_m <- eigen(cov_m)$value
#'   is_planar <- eigen_m[2] > (th1*eigen_m[3]) && (th2*eigen_m[2]) > eigen_m[1]
#'   return(list(planar = is_planar))
#' }
#'
#' # Apply a user-defined function
#' M <- point_metrics(las, ~plane_metrics1(X,Y,Z), k = 25)
#' #> Computed in 6.3 seconds
#'
#' # We can verify that it returns the same as 'shp_plane'
#' las <- segment_shapes(las, shp_plane(k = 25), "planar")
#' #> Computed in 0.1 seconds
#'
#' all.equal(M$planar, las$planar)
#'
#' # At this stage we can be clever and find that the bottleneck is
#' # the eigenvalue computation. Let's write a C++ version of it with
#' # Rcpp and RcppArmadillo
#' Rcpp::sourceCpp(code = "
#' #include <RcppArmadillo.h>
#' // [[Rcpp::depends(RcppArmadillo)]]
#'
#' // [[Rcpp::export]]
#' SEXP eigen_values(arma::mat A) {
#' arma::mat coeff;
#' arma::mat score;
#' arma::vec latent;
#' arma::princomp(coeff, score, latent, A);
#' return(Rcpp::wrap(latent));
#' }")
#'
#' plane_metrics2 = function(x,y,z, th1 = 25, th2 = 6) {
#'   xyz <- cbind(x,y,z)
#'   eigen_m <- eigen_values(xyz)
#'   is_planar <- eigen_m[2] > (th1*eigen_m[3]) && (th2*eigen_m[2]) > eigen_m[1]
#'   return(list(planar = is_planar))
#' }
#'
#' M <- point_metrics(las, ~plane_metrics2(X,Y,Z), k = 25)
#' #> Computed in 0.5 seconds
#'
#' all.equal(M$planar, las$planar)
#' # Here we can see that the optimized version is way better but is still 5-times slower
#' # because of the overhead of calling R functions and switching back and forth from R to C++.
#'
#' M <- point_eigenvalues(las, k = 25)
#' is_planar = M$eigen_medium > (25*M$eigen_smallest) & (6*M$eigen_medium) > M$eigen_largest
#'
#' # Use the filter argument to process only first returns
#' M1 <- point_metrics(las, ~plane_metrics2(X,Y,Z), k = 25, filter = ~ReturnNumber == 1)
#' dim(M1) # 13894 instead of 17182 previously.
#' }
#' @export
#' @family metrics
#' @md
point_metrics <- function(las, func, k, r,  xyz = FALSE, filter = NULL, ...) {

  if (is_disable_point_metrics())
    stop("Because of a new CRAN policy that affects numerous packages, we temporarily removed this function to give us time to find a workaround and prevent the package from being removed from CRAN.
To get a version of the package with this function enabled you can install from r-universe rather than from CRAN: https://r-lidar.r-universe.dev/lidR", call. = FALSE)

  UseMethod("point_metrics", las)
}

#' @export
point_metrics.LAS <- function(las, func, k, r, xyz = FALSE, filter = NULL, ...) {

  if (is_disable_point_metrics())
  {
    C_point_metrics = function(a, b, c, d, e, f) {}
  }

  if (missing(k) && missing(r))  stop("'k' or 'r' is missing", call. = FALSE)

  # knn + radius
  if (!missing(r) && !missing(k)) {
    assert_is_a_number(k)
    assert_is_a_number(r)
    assert_all_are_positive(k)
    assert_all_are_positive(r)
    k <- as.integer(k)
    mode <- 3L
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

  formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!formula) func <- lazyeval::f_capture(func)

  # Preparation of the objects
  func <- lazyeval::f_interp(func)
  call <- lazyeval::as_call(func)

  # Memory allocation for the query. This memory will be recycled in each iteration
  p <- list(...)
  if (is.null(p$alloc))
    n <- if (mode == 0L) k else as.integer(density(las) * pi * r^3)
  else
    n <- as.integer(p$alloc)

  if (n < 1) n <- 1

  # Creation of a call environment
  env <- new.env(parent = parent.frame())

  filter <- parse_filter(las, filter)

  output <- C_point_metrics(las, k, r, n, call, env, filter)

  if (length(output[[1]]) == 1)
  {
    name <- names(output[[1]])
    output <- data.table::data.table(unlist(output))
    if (!is.null(name)) data.table::setnames(output, name)
  }
  else
  {
    output <- data.table::rbindlist(output)
  }

  if (xyz)
  {
    xyz <- coordinates3D(las)
    data.table::setDT(xyz)
    if (length(filter) > 1 && !all(filter)) xyz <- xyz[filter]
    coln <- names(output)
    coln <- c("X", "Y", "Z", coln)
    output[["X"]] <- xyz$X
    output[["Y"]] <- xyz$Y
    output[["Z"]] <- xyz$Z
    data.table::setcolorder(output, coln)
    output[]
  }
  else
  {
    pointID <- NULL
    colnames <- data.table::copy(names(output))

    if (length(filter) > 1)
      output[, pointID := which(filter)]
    else
      output[, pointID := 1:npoints(las)]

    data.table::setcolorder(output, c("pointID", colnames))
    output[]
  }

  return(output)
}

#' @export
#' @rdname point_metrics
point_eigenvalues = function(las, k, r, xyz = FALSE, metrics = FALSE, coeffs = FALSE, filter = NULL)
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
  M <- C_eigen_metrics(las, k, r, coeffs, filter, getThreads())
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

