#' Individual tree segmentation
#'
#' Individual tree segmentation using Silva et al. (2016) algorithm (see reference).
#' This is a simple method based on local maxima + voronoi tesselation. This algorithm is implemented
#' in the package \code{rLiDAR}. This version is \emph{not} the version from \code{rLiDAR}. It is a
#' code written from scratch by the lidR author from the original paper and is considerably
#' (between 250 and 1000 times) faster.
#'
#' @param las An object of the class \code{LAS}. If missing \code{extra} is turned to \code{TRUE}
#' automatically.
#' @param extra logical. By default the function classifies the original point cloud by reference
#' and return nothing (NULL) i.e. the original point cloud is automatically updated in place. If
#' \code{extra = TRUE} an additional \code{RasterLayer} used internally can be returned.
#' @param chm RasterLayer. Image of the canopy. Can be computed with \link[lidR:grid_canopy]{grid_canopy}
#' or \link[lidR:grid_tincanopy]{grid_tincanopy} or read it from an external file.
#' @param treetops \code{RasterLayer} or \code{data.frame} containing the position of the
#' trees. Can be computed with \link[lidR:tree_detection]{tree_detection} or read from an external file.
#' @param max_cr_factor numeric. Maximum value of a crown diameter given as a proportion of the
#' tree height. Default is 0.6,  meaning 60\% of the tree height.
#' @param exclusion numeric. For each tree, pixels with an elevation lower than \code{exclusion}
#' multiplied by the tree height will be removed. Thus, this number belongs between 0 and 1.
#' @param ... Supplementary options. Currently \code{field} is supported to change the default name of
#' the new column.
#'
#' @return Nothing (NULL), the point cloud is updated by reference. The original point cloud
#' has a new column named \code{treeID} containing an ID for each point that refer to a segmented tree.
#' If \code{extra = TRUE} the function returns a \code{RasterLayer} used internally.
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' chm = grid_canopy(las, res = 0.5, subcircle = 0.3)
#' chm = as.raster(chm)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, 5, 2)
#' lastrees_silva(las, chm, ttops)
#' plot(las, color = "treeID", colorPalette = col)
#'
#' @references
#' Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers,
#' J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.)
#' Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573.
#' https://doi.org/10.1080/07038992.2016.1196582.
#' @export
#' @family  tree_segmentation
lastrees_silva = function(las, chm, treetops, max_cr_factor = 0.6, exclusion = 0.3, extra = FALSE, ...)
{
  . <- R <- X <- Y <- Z <- id <- d <- hmax <- NULL

  field = "treeID"
  p = list(...)
  if(!is.null(p$field))
    field = p$field

  stopif_forbidden_name(field)

  if (is(treetops, "RasterLayer"))
    treetops = raster::as.data.frame(treetops, xy = TRUE, na.rm = TRUE)
  else if (!is.data.frame(treetops))
    stop("'treetops' format not recognized.", call. = FALSE)

  if (max_cr_factor < 0 | max_cr_factor > 1)
    stop("'max_cr_factor' should be between 0 and 1", call. = FALSE)

  if (exclusion < 0 | exclusion > 1)
    stop("'exclusion' should be between 0 and 1", call. = FALSE)

  ttops = data.table::copy(treetops)
  data.table::setDT(ttops)
  data.table::setnames(ttops, names(ttops), c("X", "Y", "Z"))

  chmdt = data.table::setDT(raster::as.data.frame(chm, xy = TRUE, na.rm = T))
  data.table::setnames(chmdt, names(chmdt), c("X", "Y", "Z"))

  # Voronoi tesselation is nothing else than the nearest neigbour
  u = C_knn(ttops$X, ttops$Y, chmdt$X, chmdt$Y, 1L)
  chmdt[, id := u$nn.idx[,1]]
  chmdt[, d := u$nn.dist[,1]]

  chmdt[, hmax := max(Z), by = id]
  chmdt = chmdt[Z >= exclusion*hmax & d <= max_cr_factor*hmax, .(X,Y, id)]
  as.lasmetrics(chmdt, raster::res(chm)[1])
  crown = as.raster(chmdt)

  if(!missing(las))
  {
    lasclassify(las, crown, field)
    lasaddextrabytes(las, name = field, desc = "An ID for each segmented tree")
  }

  if (!extra & !missing(las))
    return(invisible())
  else
    return(crown)
}