#' Noise Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_noise}. It implements an
#' algorithm for outliers (noise) segmentation based on Statistical Outliers
#' Removal (SOR) methods first described in the PCL library
#' and also implemented in CloudCompare (see references).
#' For each point, it computes the mean distance to all its k-nearest neighbours.
#' The points that are farther than the average distance plus a number of times
#' (multiplier) the standard deviation are considered noise.
#'
#' @param k numeric. The number of neighbours
#' @param m numeric. Multiplier. The maximum distance will be: `avg distance + m * std deviation`.
#' If `quantile = TRUE`, `m` becomes the quantile threshold.
#' @param quantile boolean. Modification of the original SOR to use a quantile
#' threshold instead of a standard deviation multiplier. In this case the maximum
#' distance will be: `quantile(distances, probs = m)`
#'
#' @export
#'
#' @family noise segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#' # Add some artificial outliers because the original
#' # dataset is 'clean'
#' set.seed(314)
#' id = round(runif(20, 0, npoints(las)))
#' set.seed(42)
#' err = runif(20, -50, 50)
#' las$Z[id] = las$Z[id] + err
#'
#' las <- classify_noise(las, sor(15,7))
#' @md
#' @references
#' https://pointclouds.org/documentation/tutorials/statistical_outlier.html \cr
#' https://www.cloudcompare.org/doc/wiki/index.php?title=SOR_filter
sor = function(k = 10, m = 3, quantile = FALSE)
{
  assert_is_a_number(k)
  assert_is_a_number(m)
  assert_all_are_positive(k)
  assert_is_a_bool(quantile)

  if (!quantile)
    assert_all_are_non_negative(m)
  else
    assert_all_are_in_closed_range(m, 0, 1)

  k <- lazyeval::uq(k)
  m <- lazyeval::uq(m)
  quantile <- lazyeval::uq(quantile)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTOUT, "sor")
    dmean <- C_fast_knn_metrics(las, k, 1, getThreads())
    if (quantile) {
      th <- quantile(dmean, probs = m)
    } else {
      avg <- mean(dmean)
      std <- stats::sd(dmean)
      th <- avg + m*std
    }

    return(dmean > th)
  }

  class(f) <- c(LIDRALGORITHMOUT, LIDRALGORITHMOPENMP)
  return(f)
}

#' Noise Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_noise}. It implements an
#' algorithm for outliers (noise) segmentation based on isolated voxels filter (IVF).
#' It is similar to \href{https://rapidlasso.com/lastools/lasnoise/}{lasnoise from lastools}.
#' The algorithm finds points that have only a few other points in their surrounding
#' 3 x 3 x 3 = 27 voxels.
#'
#' @param res numeric. Resolution of the voxels
#' @param n integer. The maximal number of 'other points' in the 27 voxels
#'
#' @export
#'
#' @family noise segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#' # Add some artificial outliers
#' set.seed(314)
#' id = round(runif(20, 0, npoints(las)))
#' set.seed(42)
#' err = runif(20, -50, 50)
#' las$Z[id] = las$Z[id] + err
#'
#' las <- classify_noise(las, ivf(5,2))
ivf = function(res = 5, n = 6)
{
  assert_is_a_number(res)
  assert_is_a_number(n)
  assert_all_are_positive(res)
  assert_all_are_non_negative(n)

  res <- lazyeval::uq(res)
  n <- lazyeval::uq(n)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTOUT, "ivf")
    return(C_isolated_voxel(las, res, n))
  }

  class(f) <- c(LIDRALGORITHMOUT)
  return(f)
}

