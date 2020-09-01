#' Noise Segmentation Algorithm
#'
#' This function is made to be used in \link{classify_noise}. It implements an
#' algorithm for outliers (noise) segmentation points based on Satistical Outliers
#' Removal (SOR) methods first described in the
#' \href{https://pointclouds.org/documentation/tutorials/statistical_outlier.html}{PCL library}
#' and also implemented in
#' \href{https://www.cloudcompare.org/doc/wiki/index.php?title=SOR_filter}{CloudCompare}.
#' For each point, it computes the mean distance from it to all its k-neighbours.
#' The points that are farther than the average distance plus a number of times
#' (multiplier) the standard deviation are considered noise.
#'
#' @param k numeric. The number of neighbours
#' @param m numeric. Multiplier. The maximum distance will be: avg distance + m * std deviation
#'
#' @export
#'
#' @family noise segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # Add some artificial outliers
#' set.seed(314)
#' id = round(runif(20, 0, npoints(las)))
#' set.seed(42)
#' err = runif(20, -50, 50)
#' las$Z[id] = las$Z[id] + err
#'
#' las <- classify_noise(las, sor(15,7))
#' plot(las, color = "Classification")
sor = function(k = 10, m = 3)
{
  assert_is_a_number(k)
  assert_is_a_number(m)
  assert_all_are_positive(k)
  assert_all_are_non_negative(m)

  k <- lazyeval::uq(k)
  m <- lazyeval::uq(m)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTOUT, "sor")
    dmean <- C_fast_knn_metrics(las, k, 1, getThreads())
    avg <- mean(dmean)
    std <- sd(dmean)
    return(dmean > avg + m*std)
  }

  class(f) <- c(LIDRALGORITHMOUT, LIDRALGORITHMOPENMP)
  return(f)
}
