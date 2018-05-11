#' Individual tree segmentation
#'
#' Individual tree segmentation with several possible algorithms. The function is a wrapper around
#' all the existing methods. Considering the increasing number of tree segmentation methods available,
#' each method is now documented on its own page (see section "See Also")
#'
#' @param las An object of the class \code{LAS}. If missing, \code{extra} is turned to \code{TRUE}
#' automatically.
#' @param algorithm character. The name of an algorithm. Can be \code{"dalponte2016"},
#' \code{"watershed"},\code{"li2012"} (deprecated), \code{"li2012-2"} or \code{"silva2016"}.
#' @param ... parameters for the algorithms. These depend on the algorithm used (see documentation
#' of each method).
#'
#' @return Usually nothing (NULL). The point cloud is updated by reference (in place without copy).
#' But some algorithms may provide extra outputs. Usually it returns intermediate objects used
#' internally, such as a \code{RasterLayer} or a \code{SpatialPolygonDataFrame}.
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' # Li 2012
#' lastrees(las, "li2012-2", R = 3, speed_up = 5)
#' plot(las, color = "treeID", colorPalette = col)
#' @export
#' @family  tree_segmentation
lastrees <- function(las, algorithm, ...)
{
  stopifnotlas(las)

  if (algorithm == "dalponte2016" )
    return(lastrees_dalponte(las, ...))
  else if (algorithm == "watershed")
    return(lastrees_watershed(las, ...))
  else if (algorithm == "li2012")
    return(lastrees_li(las, ...))
  else if (algorithm == "li2012-2")
    return(lastrees_li2(las, ...))
  else if (algorithm == "silva2016")
    return(lastrees_silva(las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}