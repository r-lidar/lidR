#' Computes the polygon that encloses the points
#'
#' Computes the polygon that encloses the points. It reads all the files one by one and computes a
#' concave hull using the \link{st_concave_hull} function. When all the hulls are computed it updates the
#' LAScatalog to set the true polygons instead of the bounding boxes.
#'
#' @section Non-supported LAScatalog options:
#' The options `select`, `output files`, `chunk size`, `chunk buffer`, `chunk alignment` are not
#' supported and not respected in  `catalog_boundaries*` because the function must always process by
#' file, without buffer and knows which attributes to load.
#'
#' @param ctg A LAScatalog
#' @param ... propagated to \link{st_concave_hull}
#'
#' @return A LAScatalog with true boundaries
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile, filter = "-drop_z_below 0.5")
#' ctg2 <- catalog_boundaries(ctg, concavity = 1, length_threshold = 15)
#' plot(ctg)
#' plot(ctg2, add = TRUE)
catalog_boundaries = function(ctg, ...)
{
  if (is_lascatalog_v3(ctg)) ctg <- lascatalog_v3_repair(ctg)

  opt_chunk_buffer(ctg) <- 0
  opt_chunk_size(ctg) <- 0
  opt_select(ctg) <- "xyz"
  opt_output_files(ctg) <- ""
  opt_independent_files(ctg) <- TRUE
  res <- catalog_map(ctg, st_concave_hull, ...)

  sf::st_geometry(ctg@data) <- res
  attr(ctg, "trueshape") <- TRUE
  return(ctg)
}
