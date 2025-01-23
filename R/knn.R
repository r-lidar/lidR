#' Search Nearest Neighbors
#'
#' Fast parallelized k-neareast neighbor searching algorithms for point cloud in LAS format
#'
#' @param data LAS object for input point cloud
#' @param query LAS object for query locations
#' @param k number of nearest neighbors to search.
#'
#' @return a list contains:
#' \enumerate{
#' \item **nn.index** an n x k matrix for the nearest neighbor indice.
#' \item **nn.dist** an n x k matrix for the nearest neighbor Euclidean distances.
#' }
#' @export
knn = function(data, k = 10)
{
  stopifnotlas(data)
  assert_all_are_non_negative(k)
  return(cpp_knn(data, k, getThreads()))
}

#' @rdname knn
#' @export
knnx = function(data, query, k = 10)
{
  stopifnotlas(data)
  stopifnotlas(query)
  assert_all_are_non_negative(k)
  return(cpp_knnx(data, query, k, getThreads()))
}

