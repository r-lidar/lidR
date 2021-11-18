#' Computes the polygon that encloses the points
#'
#' Computes the polygon that encloses the points. It reads all the files one by one and computes a
#' concave hull using the \link{concaveman} function. When all the hulls are computed it updates the
#' LAScatalog to set the true polygons instead of the bounding boxes.
#'
#' @param ctg A LAScatalog
#' @param concavity,length_threshold see \link{concaveman}
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
  f = function(las, bbox, concavity, length_threshold) {
    p <- st_concave_hull(las, concavity = concavity, length_threshold = length_threshold)
    return(p)
  }

  if (is_lascatalog_v3(ctg)) ctg <- lascatalog_v3_repair(ctg)

  opt_chunk_buffer(ctg) <- 0
  opt_chunk_size(ctg) <- 0
  opt_select(ctg) <- "xyz"
  opt_output_files(ctg) <- ""
  opt_independent_files(ctg) <- TRUE
  options <- list(autoread = TRUE)
  res <- catalog_apply(ctg, f, concavity = concavity, length_threshold = length_threshold, .options = options)

  geom <- do.call(c, res)
  sf::st_geometry(ctg@data) <- geom
  attr(ctg, "trueshape") <- TRUE
  return(ctg)
}
