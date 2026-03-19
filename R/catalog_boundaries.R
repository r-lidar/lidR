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

#' Split a catalog into spatially clustered subsets
#'
#' This function groups elements of a catalog into spatial clusters based on
#' contiguity of the tiles. The original catalog is then split into `n` catalogs
#'
#' @param ctg A LAScatalog
#' @param buffer Numeric. Buffer distance applied to tile geometry before clustering. Defaults to 1.
#'
#' @return A list of LAScatalog
#'
#' @export
catalog_split_clusters = function(ctg, buffer = 1)
{
  sfctg = sf::st_as_sf(ctg)
  sfctg = sf::st_buffer(sfctg, buffer)
  clustered = sf::st_union(sfctg)
  clustered = sf::st_cast(clustered, "POLYGON")
  idx = sf::st_intersects(clustered, sfctg)
  subctg = lapply(idx, function(i) ctg[i,])
  subctg
}
