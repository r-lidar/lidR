#' Computes the polygon that encloses the points
#'
#' Computes the polygon that encloses the points. It reads all the file one by one and computes a
#' concave hull using the concaveman function. When all the hulls are computed it updates the
#' LAScatalog to set the true polygons instead of the bounding boxes.
#'
#' @param ctg A LAScatalog
#' @param concavity numeric a relative measure of concavity. 1 results in a relatively detailed shape,
#' Infinity results in a convex hull. You can use values lower than 1, but they can produce pretty crazy
#' shapes.
#' @param lengthThreshold numeric. when a segment length is under this threshold, it stops being
#' considered for further detalization. Higher values result in simpler shapes.
#' @param simplify numeric: passed to \link[rgeos:gSimplify]{gSimplify}.
#'
#' @section Supported processing options:
#' Supported processing options for more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item chunk size: Not supported, it processes by file.
#' \item chunk buffer: Not supported, it processes by file with no buffer.
#' \item chunk alignment: Not supported, it processes by file.
#' \item \strong{progress}: Displays a progress estimate.
#' \item output files: Not supported, it returns an R object
#' \item select: Not supported, it loads XYZ only.
#' \item \strong{filter}: Read only the points of interest.
#' }
#'
#' @return A LAScatalog with true boundaries
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile, filter = "-drop_z_below 0.5")
#' ctg2 <- catalog_boundaries(ctg, 2, 5, 3)
#' plot(ctg)
#' plot(ctg2, add = TRUE)
catalog_boundaries = function(ctg, concavity = 5, lengthThreshold = 5, simplify = 3)
{
  f = function(las, bbox, concavity, lengthThreshold, simplify)
  {
    cc <- concaveman::concaveman(as.matrix(coordinates(las)), concavity = concavity, length_threshold = lengthThreshold)
    p <- sp::Polygon(cc)
    p <- sp::Polygons(list(p), ID = "1")
    p <- sp::SpatialPolygons(list(p), proj4string = las@proj4string)
    if (simplify > 0)  p <- rgeos::gSimplify(p, simplify)
    return(p)
  }

  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_select(ctg) <- "xyz"
  lidR::opt_output_files(ctg) <- ""
  options = list(autoread = TRUE)
  res = lidR::catalog_apply(ctg, f, concavity = concavity, lengthThreshold = lengthThreshold, simplify = simplify, .options = options)

  for (i in 1:length(res)) res[[i]]@polygons[[1]]@ID = as.character(i)
  Sr <- do.call(rbind, res)
  data <- ctg@data

  nctg <- new("LAScatalog")
  nctg@bbox <- Sr@bbox
  nctg@proj4string <- Sr@proj4string
  nctg@plotOrder <- Sr@plotOrder
  nctg@data <- data
  nctg@polygons <- Sr@polygons
  lidR:::opt_copy(nctg) <- ctg
  attr(nctg, "trueshape") <- TRUE
  return(nctg)
}
