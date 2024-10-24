#' Connected-Component Labeling
#'
#' Assigns an ID to each cluster of connected components in a point cloud. The point cloud is subdivided
#' into a 3D grid, and a classical [Connected-Component Labeling](https://en.wikipedia.org/wiki/Connected-component_labeling)
#' algorithm is applied in 3D.
#'
#' @param las A LAS object representing the point cloud data.
#' @param res Grid resolution. If two non-empty voxels are contiguous, they are considered part of
#' the same component.
#' @param min_pts Minimum number of points in a cluster. If a cluster contains fewer than `min_pts`
#' points, it is assigned an ID of 0, indicating that it does not belong to any group.
#' @param name A string specifying the name of the new attribute used to store the ID of each cluster.
#'
#' @return A LAS object with an additional attribute named as specified by `name`.
#' @md
#' @export
connected_components = function(las, res, min_pts, name = "clusterID")
{
  .N <- N <- clusterID <- gpstime <- NULL

  u = C_connected_component(las, res)
  las = add_lasattribute(las, u, name, "connected component ID")
  grp = las@data[, .N, by = clusterID]
  grp = grp[N < min_pts]
  invalid = las@data[[name]] %in% grp$clusterID
  las@data[[name]][invalid] = 0L
  return(las)
}



#' Computes the Distance to k-Nearest Neighbors
#'
#' Computes the average distance between each point and its k-nearest neighbors in a point cloud.
#' The results are stored in a new attribute.
#'
#' @param las A LAS object representing the point cloud data.
#' @param k The number of nearest neighbors.
#' @param name A string specifying the name of the new attribute used to store the computed distances.
#'
#' @return A LAS object with an additional attribute named as specified by `name`.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-inside 481250 3812980 481300 3813030")
#' las = knn_distance(las)
#' #plot(las, color = "distance", breaks = "quantile", legend = TRUE)
knn_distance = function(las, k = 10, name = "distance")
{
  d = C_knn_distance(las, k, getThreads())
  las = add_lasattribute(las, d, name, "knn average distance")
  return(las)
}

