#' Individual tree segmentation
#'
#' Individual tree segmentation using watersheds and marker controled watershed.
#'
#' \strong{Simple watershed} is based on the bioconductor package \code{EBIimage}. You need to install this package
#' to run this method (see its \href{https://github.com/aoles/EBImage}{github page}). Internally the
#' function EBImage::watershed is called.\cr\cr
#' \strong{Marker controlled watershed} is based on the \code{imager} package. Internally the
#' function \link[imager:watershed]{imager::watershed} is called using the tree tops as priority map.
#'
#' @param las An object of the class \code{LAS}. If missing \code{extra} is turned to \code{TRUE}
#' automatically.
#' @param extra logical. By default the function classifies the original point cloud by reference
#' and return nothing (NULL) i.e. the original point cloud is automatically updated in place. If
#' \code{extra = TRUE} an additional \code{RasterLayer} used internally can be returned.
#' @template param-chm-lastrees
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
#' @param tol numeric. Tolerance see ?EBImage::watershed.
#' @param ext numeric. see ?EBImage::watershed.
#' @template param-treetops
#' @param field character. If the \code{SpatialPointsDataFrame} contains an attribute with the ID for
#' each tree, the name of this column. This way, original IDs will be preserved. If there is no scuh data
#' trees will be numbered sequentially.
#'
#' @return Nothing (NULL), the point cloud is updated by reference. The original point cloud
#' has a new column named \code{'treeID'} (or something else if the \code{field} option has been changed)
#' containing an ID for each point that refer to a segmented tree.
#' If \code{extra = TRUE} the function returns a \code{RasterLayer} used internally.
#'
#' @export
#' @family  tree_segmentation
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(250)
#'
#' chm = grid_canopy(las, "p2r", res = 0.5, subcircle = 0.3)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, "lmf", 4, 2)
#' crowns = lastrees_mcwatershed(las, chm, ttops, extra = TRUE)
#'
#' \dontrun{
#' plot(las, color = "treeID", colorPalette = col)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#'
#' raster::plot(crowns, col = pastel.colors(250))
#' sp::plot(ttops, add = TRUE)
#' }
lastrees_watershed = function(las, chm, th_tree = 2, tol = 1, ext = 1, extra = FALSE, field = "treeID")
{
  lastrees_ws_generic(las, chm, th_tree = th_tree, tol = tol, ext = ext, extra = extra, field = field)
}

#' @rdname lastrees_watershed
#' @export
lastrees_mcwatershed = function(las, chm, treetops, th_tree = 2, extra = FALSE, field = "treeID")
{
  lastrees_ws_generic(las, chm, th_tree = th_tree, treetops = treetops, extra = extra, field = field)
}

lastrees_ws_generic = function(las, chm, th_tree = 2, tol = 1, ext = 1, treetops = NULL, extra = FALSE, field = "treeID")
{
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_a_number(th_tree)
  assertive::assert_is_a_number(tol)
  assertive::assert_is_a_number(ext)
  assertive::assert_is_a_bool(extra)

  # Test if requiered packages are installed
  if (is.null(treetops))
  {
    if (!requireNamespace("EBImage", quietly = TRUE))
      stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)
  }
  else
  {
    if (!requireNamespace("imager", quietly = TRUE))
      stop("'imager' package is needed for this function to work.", call. = F)
  }

  # Convert the CHM to a matrix
  Canopy <- raster::as.matrix(chm)
  mask   <- Canopy < th_tree | is.na(Canopy)
  Canopy[mask] <- 0

  # Watershed
  if (is.null(treetops))
  {
    Crowns = EBImage::watershed(Canopy, tol, ext)
  }
  # Marker controlled watershed
  else
  {
    X = match_chm_and_seeds(chm, treetops, field)
    cells = X$cells
    ids = X$ids

    seeds = chm
    seeds[] = 0L
    seeds[cells] = ids
    treetops = raster::as.matrix(seeds)

    Canopy <- imager::as.cimg(Canopy)
    treetops  <- imager::as.cimg(treetops)
    Crowns <- imager::watershed(treetops, Canopy)
    Crowns <- Crowns[,,1,1]
    storage.mode(Crowns) = "integer"
  }

  Crowns[mask] <- NA_integer_
  Crowns = raster::raster(Crowns)
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