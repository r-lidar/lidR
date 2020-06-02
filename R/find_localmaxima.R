#' Local Maximum Filter
#'
#' Generic local maximum filter. For individual tree detection use \link{find_trees} with the
#' \link{lmf} algorithm that is more adequate for ITD. This function is a more generic method for
#' multiple purposes other than tree segmentation. This function is natively parallelized with OpenMP.
#'
#' @param las An object of class LAS
#' @param w numeric. Window shape. 1 number for the diameter of a disc, 2 numbers for a rectangle
#' (width, height), 3 numbers for an oriented rectangle (width, height, angle). The angle must be
#' in radians.
#' @param filter formula. Memory efficient way to work only with a subset of the data without creating a copy
#' of the data.
#' @return \code{SpatialPointsDataFrame} with attributes from the corresponding point in the LAS object,
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzi", filter = "-drop_z_below 0")
#'
#' # Using a 20x5 rectangle with a 45 degrees angle.
#' # This won't find the tree properly in the general case
#' # but may find some oriented structure.
#' lm = find_localmaxima(las, c(20, 5, pi/4))
#' @export
find_localmaxima = function(las, w, filter = NULL)
{
  # 1 circular
  # 2 rectangular
  # 3 custom (not supported yet)
  assert_all_are_positive(w[1])
  if (!is.na(w[2])) assert_all_are_positive(w[2])
  if (!is.na(w[3])) assert_all_are_in_open_range(w[3], -pi/2, pi/2)
  if (length(w) == 0 || length(w) > 3) stop("Invalid window.", call. = FALSE)

  if (length(w) == 1) verbose("Local maxima search in a disc")
  if (length(w) == 2) verbose("Local maxima search in a rectangle")
  if (length(w) == 3) verbose("Local maxima search in an oriented rectangle")

  filter <- parse_filter(las, filter)
  id <- C_local_maximum(las, w, getThreads())

  maxima <- las@data[id]
  data.table::setDF(maxima)

  if (nrow(maxima) == 0) {
    coords <- matrix(0, ncol = 2)
    data   <- las@data[1, -c("X", "Y")]
    output <- sp::SpatialPointsDataFrame(coords, data, proj4string = las@proj4string)
    output <- output[0,]
  } else {
    output <- maxima
    sp::coordinates(output) <- ~X+Y
    raster::crs(output) <- crs(las)
  }

  output@bbox <- sp::bbox(las)
  return(output)
}
