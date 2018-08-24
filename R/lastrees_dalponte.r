#' Individual tree segmentation
#'
#' Individual tree segmentation using Dalponte et al. (2016) algorithm (see reference).
#' This is a local maxima + growing region algorithm. It is based on the constraints proposed by
#' Dalponte and Coomes (see references). This algorithm exists in the package \code{itcSegment}.
#' This version is identical to the original but with superfluous code removed and rewritten
#' efficiently. Consequently it is hundreds to millions times faster. Note that this algorithm strictly
#' performs a segmentation, while the original method as implemented in \code{itcSegment} and described
#' in the manuscript also performs a pre- and post-process when these tasks are expected to be done
#' by the user in separate functions.
#'
#' @param las An object of the class \code{LAS}. If missing \code{extra} is turned to \code{TRUE}
#' automatically.
#' @param extra logical. By default the function classifies the original point cloud by reference
#' and return nothing (NULL) i.e. the original point cloud is automatically updated in place. If
#' \code{extra = TRUE} an additional \code{RasterLayer} used internally can be returned.
#' @template param-chm-lastrees
#' @template param-treetops
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the tree height multiplied by this value.
#' It should be between 0 and 1. Default 0.45.
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the current mean height of the region
#' multiplied by this value. It should be between 0 and 1. Default 0.55.
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in pixels).
#' Default 10.
#' @param field character. If the \code{SpatialPointsDataFrame} contains an attribute with the ID for
#' each tree, the name of this column. This way, original IDs will be preserved. If there is no scuh data
#' trees will be numbered sequentially.
#'
#' @return Nothing (NULL), the point cloud is updated by reference. The original point cloud
#' has a new column named after the string provided in \code{field} containing an ID for each point
#' that refer to a segmented tree. If \code{extra = TRUE} algorithms return a \code{RasterLayer} used
#' internally.
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' chm = grid_canopy(las, "p2r", res = 0.5, subcircle = 0.3)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, "lmf", 4, 2)
#' lastrees_dalponte(las, chm, ttops)
#' plot(las, color = "treeID", colorPalette = col)
#'
#' @references
#' Dalponte, M. and Coomes, D. A. (2016), Tree-centric mapping of forest carbon density from
#' airborne laser scanning and hyperspectral data. Methods Ecol Evol, 7: 1236–1245. doi:10.1111/2041-210X.12575.
#' @export
#' @family  tree_segmentation
lastrees_dalponte = function(las, chm, treetops, th_tree = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, extra = FALSE, field = "treeID")
{
  stopifnotlas(las)
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_all_of(treetops, "SpatialPointsDataFrame")
  assertive::assert_is_a_number(th_tree)
  assertive::assert_is_a_number(th_seed)
  assertive::assert_is_a_number(th_cr)
  assertive::assert_is_a_number(max_cr)
  assertive::assert_is_a_bool(extra)
  assertive::assert_all_are_in_closed_range(th_seed, 0, 1)
  assertive::assert_all_are_in_closed_range(th_cr, 0, 1)

  X     <- match_chm_and_seeds(chm, treetops, field)
  cells <- X$cells
  ids   <- X$ids

  rtreetops   <- raster::raster(chm)
  rtreetops[] <- 0L
  suppressWarnings(rtreetops[cells] <- ids)

  Canopy <- raster::as.matrix(chm)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[is.na(Canopy)] <- -Inf

  Maxima <- raster::as.matrix(rtreetops)
  Maxima <- t(apply(Maxima, 2, rev))

  Crowns <- C_lastrees_dalponte(Canopy, Maxima, th_seed, th_cr, th_tree, max_cr)
  Maxima[Maxima == 0L] <- NA_integer_
  Crowns[Crowns == 0L] <- NA_integer_

  Crowns <- raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) <- raster::extent(chm)

  if(!missing(las))
  {
    lasclassify(las, Crowns, field)
    lasaddextrabytes(las, name = field, desc = "An ID for each segmented tree")
  }

  if (!extra & !missing(las))
    return(invisible(NULL))
  else
    return(Crowns)
}