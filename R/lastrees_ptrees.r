#' Individual tree segmentation
#'
#' @param las An object of the class \code{LAS}.
#' @param k integer vector. A serie of k-nearest neighbors to use.
#' @param ... Supplementary options. Currently field is supported to change the default name of the
#' new column.
#'
#' @return Nothing (\code{NULL}), the point cloud is updated by reference. The original point cloud
#' has a new column named treeID containing an ID for each point that refer to a segmented tree.
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 5")
#'
#' k_values = c(5,6,7,8,10,12,15,20,25,30,40,60,80,100)
#' lastrees_ptrees(las, 10)
lastrees_ptrees = function(las, k, ...)
{
  stopifnotlas(las)
  stopifnot(is.numeric(k), !any(k <= 0))

  field = "treeID"
  p = list(...)
  if(!is.null(p$field))
    field = p$field

  id = C_lastrees_ptrees(las, k)
  lasaddextrabytes(las, id, field, "An ID for each segmented tree")

  return(invisible())
}






