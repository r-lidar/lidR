#' Computes the polygon that encloses the points
#'
#' Computes the polygon that encloses the points. It reads all the files one by one and computes a
#' concave hull using the \link{concaveman} function. When all the hulls are computed it updates the
#' LAScatalog to set the true polygons instead of the bounding boxes.
#'
#' @param ctg A LAScatalog
#' @param concavity numeric. A relative measure of concavity. 1 results in a relatively detailed shape,
#' Infinity results in a convex hull. You can use values lower than 1, but they can produce pretty crazy
#' shapes.
#' @param length_threshold numeric. When a segment length is under this threshold, it stops being
#' considered for further detailed processing. Higher values result in simpler shapes.
#'
#' @section Supported processing options:
#' Supported processing options for more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item chunk size: Not supported, it processes by file.
#' \item chunk buffer: Not supported, it processes by file with no buffer.
#' \item chunk alignment: Not supported, it processes by file.
#' \item \strong{progress}: Displays a progress estimate.
#' \item output files: Not supported, it returns an R object.
#' \item select: Not supported, it loads XYZ only.
#' \item \strong{filter}: Read only the points of interest.
#' }
#'
#' @return A LAScatalog with true boundaries
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile, filter = "-drop_z_below 0.5")
#' ctg2 <- catalog_boundaries(ctg, 1, 15)
#' plot(ctg)
#' plot(ctg2, add = TRUE)
catalog_boundaries = function(ctg, concavity = 5, length_threshold = 5)
{
  f = function(las, bbox, concavity, length_threshold)
  {
    cc <- concaveman(las$X, las$Y, concavity = concavity, length_threshold = length_threshold)
    p <- sp::Polygon(cc)
    p <- sp::Polygons(list(p), ID = "1")
    p <- sp::SpatialPolygons(list(p), proj4string = las@proj4string)
    return(p)
  }

  lidR::opt_chunk_buffer(ctg) <- 0
  lidR::opt_chunk_size(ctg) <- 0
  lidR::opt_select(ctg) <- "xyz"
  lidR::opt_output_files(ctg) <- ""
  lidR::opt_independent_files(ctg) <- TRUE
  options = list(autoread = TRUE)
  res = lidR::catalog_apply(ctg, f, concavity = concavity, length_threshold = length_threshold, .options = options)

  for (i in 1:length(res)) res[[i]]@polygons[[1]]@ID = as.character(i)
  Sr <- do.call(rbind, res)
  data <- ctg@data

  nctg <- new("LAScatalog")
  nctg@bbox <- Sr@bbox
  nctg@proj4string <- Sr@proj4string
  nctg@plotOrder <- Sr@plotOrder
  nctg@data <- data
  nctg@polygons <- Sr@polygons
  opt_copy(nctg) <- ctg
  attr(nctg, "trueshape") <- TRUE
  return(nctg)
}
