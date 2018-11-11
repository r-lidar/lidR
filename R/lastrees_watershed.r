#' Individual tree segmentation
#'
#' Individual tree segmentation using a simple watershed. This method is a
#' \href{https://en.wikipedia.org/wiki/Watershed_(image_processing)}{watershed segmentation}
#' method. It is based on the bioconductor package \code{EBIimage}. You need to install this package
#' to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#'
#' @param las An object of the class \code{LAS}. If missing \code{extra} is turned to \code{TRUE}
#' automatically.
#' @param extra logical. By default the function classifies the original point cloud by reference
#' and return nothing (NULL) i.e. the original point cloud is automatically updated in place. If
#' \code{extra = TRUE} an additional \code{RasterLayer} used internally can be returned.
#' @param chm RasterLayer. Image of the canopy. Can be computed with \link[lidR:grid_canopy]{grid_canopy}
#' or \link[lidR:grid_tincanopy]{grid_tincanopy} or read it from an external file.
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
#' @param tol numeric. Tolerance see ?EBImage::watershed.
#' @param ext numeric. see ?EBImage::watershed.
#' @param ... Supplementary options. Currently \code{field} is supported to change the default name of
#' the new column.
#'
#' @return Nothing (NULL), the point cloud is updated by reference. The original point cloud
#' has a new column named \code{'treeID'} (or something else if the \code{field} option has been changed)
#' containing an ID for each point that refer to a segmented tree.
#' If \code{extra = TRUE} the function returns a \code{RasterLayer} used internally.
#'
#' @export
#' @family  tree_segmentation
lastrees_watershed = function(las, chm, th_tree = 2, tol = 1, ext = 1, extra = FALSE, ...)
{
  field = "treeID"
  p = list(...)
  if(!is.null(p$field))
    field = p$field

  stopif_forbidden_name(field)

  if (!requireNamespace("EBImage", quietly = TRUE))
    stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)

  Canopy <- raster::as.matrix(chm)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[Canopy < th_tree] <- NA

  Crowns = EBImage::watershed(Canopy, tol, ext)

  Crowns[is.na(Canopy)] <- NA
  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(chm)

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