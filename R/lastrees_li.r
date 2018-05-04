#' Individual tree segmentation (deprecated)
#'
#' Individual tree segmentation using Li et al. (2012) algorithm (see reference). This method is deprecated.
#' Use \link{lastrees_li2} instead which is a slighly closer implementation of the original paper.
#'
#' This is a growing region method working at the point cloud level. It is an implementation of the
#' Li et al. 2012 (see references) algorithm made by the \code{lidR} author but with the addition of
#' a parameter \code{hmin} to stop the over-segmentation for objects that are too low. The classification
#' is done at the point cloud level and the function always returns nothing (NULL). The original
#' point cloud is updated in place with an ID for each point in a new column \code{treeID}. The user
#' is free to post-process this output the way he want.
#'
#' @param las An object of the class \code{LAS}.
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012). Default 1.5.
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012). Default 2.
#' @param hmin numeric.  Minimum height of a detected tree. Default 2.
#' @param Zu numeric. If point elvation is greater than Zu, \code{dt2} is used otherwise \code{dt1} is used.
#' See reference page 79 in Li et al. (2012). Default 15.
#' @param R numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method. Default is 10.
#' @param ... Supplementary options. Currently \code{field} is supported to change the default name of
#' the new column.
#'
#' @return Nothing (NULL), the point cloud is updated by reference. The original point cloud
#' has a new column named \code{treeID} containing an ID for each point that refer to a segmented tree.
#'
#' @references
#' Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A new method for segmenting individual
#' trees from the lidar point cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75-84.
#' @export
lastrees_li = function(las, dt1 = 1.5, dt2 = 2, Zu = 15, hmin = 2, R = 10, ...)
{
  .Deprecated("lastree_li2")
  stopifnotlas(las)

  if (dt1 <= 0) stop("dt1 should be positive",  call. = FALSE)
  if (dt1 <= 0) stop("dt1 should be positive", call. = FALSE)
  if (Zu <= 0)  stop("Zu should be positive", call. = FALSE)
  if (hmin <= 0)stop("hmin should be positive", call. = FALSE)
  if (R <= 0)   stop("R should be positive", call. = FALSE)

  field = "treeID"
  p = list(...)
  if(!is.null(p$field))
    field = p$field

  stopif_forbidden_name(field)

  if (las@header@PHB$`Max Z` < hmin)
  {
    id = rep(NA_integer_, nrow(las@data))
    warning("'hmin' is higher than the highest point. No tree segmented.")
  }
  else
  {
    progress <- LIDROPTIONS("progress")
    id = C_lastrees_li(las, dt1, dt2, Zu, hmin, R, progress)
  }

  lasaddextrabytes(las, id, field, "An ID for each segmented tree")

  return(invisible())
}