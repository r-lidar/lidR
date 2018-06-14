#' Individual tree segmentation
#'
#' The Ptrees segmentation algorithm proposed by Vega et al. (2014) (see references and section details).
#' the function \code{tree_detection} run only the fisrt part of the method i.e. the detection of the
#' trees while \code{lastrees_ptrees} performs the whole segmentation including the tree detection (see
#' details).
#'
#' This function has been written by the \code{lidR} authors from the original article. We made our
#' best to implement as far as possible exactly what is written in the original paper but we
#' cannot affirm that it is this exact original algorithm. Also, minor variations were introduced to
#' fix some issues that were not adressed in the original paper.
#' \itemize{
#' \item Addition of the parameter \code{hmin}: to reduce oversegmentation we introduced a minium height
#' threshold. Point below this thresold cannot initiate new trees during the tree detection and cannot
#' incrase a crown hull during the segmentation. This way the ground is not segmented but the segmentation
#' of each tree is prolongated to the ground anyway.
#' \item Addition of the parameter \code{nmax}: in the original article page 103 figures 5, the number
#' of possible combination is 2^n-n-1. This exponential number of combinations lead, in some cases
#' to an infinite computation time. During the developpement we got cases where the number of combinations
#' to consider was beyond a billion. If the number of tree to consider between two scales is greater
#' than \code{nmax} (i.e. the number of combination is greater than 2^nmax-nmax-1) then the "TreeSegment"
#' from the biggest scale is retained anyway, the smallest scale is considered as dummy.
#' }
#' Notice that to use the PTree strictly as originally described, the point cloud should
#' not be normalized (see reference). In that case the parameter '\code{hmin}' is miningless and can
#' be set to \code{-Inf} for example.
#'
#' @param las An object of the class \code{LAS}.
#' @param k integer vector. A serie of k-nearest neighbors to use. In this original paper a k refers
#' to a 'scale' of analyse (see reference).
#' @param hmin scalar. This is an addition from the original paper to limit oversegmentation.
#' Point below this thresold cannot initiate new trees or increase a hull (see details). Set to \code{-Inf}
#' to strictly respect original paper.
#' @param nmax integer. This is an addition from the original paper to protect against uncomputable
#' cases (see details). Set to \code{+Inf} to strictly respect the original paper (not recommended)
#' @param ... Supplementary options. Currently 'field' is supported to change the default name of the
#' new column.
#'
#' @return \itemize{
#' \item \code{tree_detection_ptrees} returns a \code{data.table} with the coordinates of the tree tops
#' (X, Y, Z)
#' \item \code{lastrees_ptrees} returns nothing (\code{NULL}), the point cloud is updated by reference.
#' The original point cloud has a new column named 'treeID' containing an ID for each point that refer
#' to a segmented tree.
#' }
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 5")
#'
#' k = c(30,20,15,10)
#' lastrees_ptrees(las, k)
#'
#' plot(las, color = "treeID", colorPalette = pastel.colors(150))
#' @family tree_segmentation
lastrees_ptrees = function(las, k, hmin = 3, nmax = 7L, ...)
{
  stopifnotlas(las)
  assertive::assert_is_numeric(k)
  assertive::assert_all_are_positive(k)
  assertive::assert_all_are_whole_numbers(k)
  assertive::assert_is_a_number(nmax)
  assertive::assert_all_are_whole_numbers(nmax)

  field = "treeID"
  p = list(...)
  if(!is.null(p$field))
    field = p$field

  TreeSegments = C_lastrees_ptrees(las, k, hmin, nmax, TRUE)
  lasaddextrabytes(las, TreeSegments$treeID, field, "An ID for each segmented tree")
  return(invisible())
}
