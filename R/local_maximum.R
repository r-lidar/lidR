#' Local Maximum Filter
#'
#' Generic local maximum filter. For individual tree detection use \link{tree_detection} with the
#' \link{lmf} algorithm that is more adequate for ITD. This function is a more generic method for
#' multiple purpose.
#'
#' @param las An object of class LAS
#' @param w numeric. Window shape. 1 number for the diameter of a disc, 2 numbers for a rectangle
#' (width, height), 3 numbers for an oriented rectangle (width, height, angle). The angle must be
#' in radian.
#' @filter formula. Memory efficient way to work only with a subset of the data without creating a copy
#' of the data.
#' @noRd
#' @export
local_maximum = function(las, w, filter = NULL)
{
  # 1 circular
  # 2 rectangular
  # 3 custom (not supported yet)
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
    data   <- data.frame(treeID = integer(1), Z = numeric(1))
    output <- sp::SpatialPointsDataFrame(coords, data, proj4string = las@proj4string)
    output <- output[0,]
  } else {
    output <- maxima
    sp::coordinates(output) <- ~X+Y
    raster::projection(output) <- raster::projection(las)
  }

  output@bbox <- sp::bbox(las)
  return(output)
}
