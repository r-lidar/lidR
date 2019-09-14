#' Individual tree segmentation
#'
#' Individual tree segmentation with several possible algorithms. The returned point cloud has a new
#' extra byte attribute named after the parameter \code{attribute} independently of the algorithm used.
#'
#' @param las An object of class \link[lidR:LAS-class]{LAS}.
#'
#' @param algorithm function. An algorithm of individual tree segmentation. \code{lidR} has:
#' \link{dalponte2016}, \link{watershed}, \link{mcwatershed}, \link{li2012} and \link{silva2016}.
#' More experimental algorithms may be found in the package \href{https://github.com/Jean-Romain/lidRplugins}{lidRplugins}
#'
#' @param attribute character. The returned LAS object as a new extra byte attribute (in a new column).
#' This parameter controls the name of the new attribute. Default is \code{"treeID"}.
#'
#' @return An object of the class \code{LAS}
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # Using Li et al. (2012)
#' las <- lastrees(las, li2012(R = 3, speed_up = 5))
#' plot(las, color = "treeID")
lastrees = function(las, algorithm, attribute = "treeID")
{
  UseMethod("lastrees", las)
}

#'@export
lastrees.LAS = function(las, algorithm, attribute = "treeID")
{
  stopif_forbidden_name(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_its(algorithm)
  lidR.context <- "lastrees"

  if (is(algorithm, "RasterBased"))
    output <- algorithm()
  else if (is(algorithm, "PointCloudBased"))
    output <- algorithm(las)
  else
    stop("Invalid algorithm provided in lastrees. The algorithm must have a class 'RasterBased' or 'PointCloudBased'")

  if (is(output, "RasterLayer"))
    las <- lasmergespatial(las, output, attribute)
  else if (is.integer(output))
    las <- lasadddata(las, output, attribute)
  else
    stop(glue::glue("Wrong output type for the algorithm used. Expected 'RasterLayer' or 'integer', received {class(output)}"))

  las <- lasaddextrabytes(las, name = attribute, desc = "An ID for each segmented tree")
  return(las)
}
