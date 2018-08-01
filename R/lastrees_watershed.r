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
#' ttops = tree_detection(chm, "lmf", 4, 2)
#' lastrees_mcwatershed(las, chm, ttops)
#' plot(las, color = "treeID", colorPalette = col)
lastrees_watershed = function(las, chm, th_tree = 2, tol = 1, ext = 1, extra = FALSE, ...)
{
  lastrees_ws_generic(las, chm, th_tree = th_tree, tol = tol, ext = ext, extra = extra)
}

#' @rdname lastrees_watershed
#' @export
lastrees_mcwatershed = function(las, chm, ttops, th_tree = 2, extra = FALSE, ...)
{
  lastrees_ws_generic(las, chm, th_tree = th_tree, ttops = ttops, extra = extra)
}

lastrees_ws_generic = function(las, chm, th_tree = 2, tol = 1, ext = 1, ttops = NULL, extra = FALSE, ...)
{
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_a_number(th_tree)
  assertive::assert_is_a_number(tol)
  assertive::assert_is_a_number(ext)
  assertive::assert_is_a_bool(extra)

  # Test if requiered packages are installed
  if (is.null(ttops))
  {
    if (!requireNamespace("EBImage", quietly = TRUE))
      stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)
  }
  else
  {
    if (!requireNamespace("imager", quietly = TRUE))
      stop("'imager' package is needed for this function to work.", call. = F)
  }

  # Change the default column for storing tree IDs
  field = "treeID"
  p = list(...)
  if(!is.null(p$field)) field = p$field
  stopif_forbidden_name(field)

  # Convert the CHM to a matrix
  Canopy <- raster::as.matrix(chm)
  mask   <- Canopy < th_tree | is.na(Canopy)
  Canopy[mask] <- 0

  # Watershed
  if (is.null(ttops))
  {
    Crowns = EBImage::watershed(Canopy, tol, ext)
  }
  # Marker controlled watershed
  else
  {
    if (is.data.frame(ttops))
    {
      seeds = chm
      seed[] = 0
      cells = raster::cellFromXY(chm, treetops[,1:2])
      seeds[cells] = 1:length(cells)
      ttops = seeds
    }

    if (is(ttops, "RasterLayer"))
    {
      ttops <- raster::as.matrix(ttops)
      ttops[is.na(ttops)] = 0
    }
    else
      stop("'ttops is of wrong type.", call. = FALSE)

    Canopy <- imager::as.cimg(Canopy)
    ttops  <- imager::as.cimg(ttops)
    Crowns <- imager::watershed(ttops, Canopy)
    Crowns <- Crowns[,,1,1]

  }

  Crowns[mask] <- NA
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