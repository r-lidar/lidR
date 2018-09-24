#' Individual tree segmentation
#'
#' Individual tree segmentation with several possible algorithms. The returned point cloud has a new
#' extra byte attribute named after the parameter \code{attribute} independently of the algorithm used.
#'
#' @param las An object of the class \link[lidR:LAS-class]{LAS}.
#'
#' @param algorithm function. An algorithm of individual tree segmentation. \code{lidR} have:
#' \link{dalponte2016}, \link{watershed}, \link{mcwatershed}, \link{li2012}, \link{hamraz2016},
#' \link{silva2016} or \link{ptrees}.
#'
#' @param attribute character. The returned LAS object as a new extra byte attribute (in a new columns).
#' This parameter controls the name of the new attribute. Default is \code{"treeID"}.
#'
#' @return An object of the class \code{LAS}
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col <- pastel.colors(200)
#'
#' # Using Li et al. (2012)
#' las <- lastrees(las, li2012(R = 3, speed_up = 5))
#' plot(las, color = "treeID", colorPalette = col)
lastrees = function(las, algorithm, attribute = "treeID")
{
  stopif_forbidden_name(attribute)

  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "IndividualTreeSegmentation"))
    stop("The algorithm is not an algorithm for individual tree segmentation", call. = FALSE)

  lidR.context <- "lastrees"

  if (is(algorithm, "RasterBased"))
    output <- algorithm()
  else if (is(algorithm, "PointCloudBased"))
    output <- algorithm(las)
  else
    stop("Invalid algorithm provided in lastrees. The algorithm must have a class 'RasterBased' or 'PointCloudBased'", call. = FALSE)

  if (is(output, "RasterLayer"))
    las@data[[attribute]] <- classify_from_rasterlayer(las, output)
  else if (is.integer(output))
    las@data[[attribute]] <- output
  else
    stop(glue::glue("Wrong output type for the algorithm used. Expected 'RasterLayer' or 'integer', received {class(output)}"), call. = FALSE)

  las <- lasaddextrabytes(las, name = attribute, desc = "An ID for each segmented tree")
  return(las)
}