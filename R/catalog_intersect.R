#' Subset a LAScatalog with a spatial object
#'
#' Subset a LAScatalog with a spatial object to keep only the tiles of interest. It can be
#' used to select tiles of interest that encompass spatial objects.
#'
#' @param ctg A \link[=LAScatalog-class]{LAScatalog} object
#' @param y  `bbox`, `sf`, `sfc`, `Extent`, `Raster*`, `Spatial*` objects
#' @param ... ignored
#' @param subset character. By default subset of the catalog. It is also possible to flag
#' the files to maintain the catalog as a whole but process only a subset of its content.
#' \code{flag_unprocessed} flags the files that will not be processed.
#' \code{flag_processed} flags the files that will be processed.
#'
#' @return A LAScatalog
#'
#' @export
catalog_intersect = function(ctg, y, ..., subset = c("subset", "flag_unprocessed", "flag_processed"))
{
  assert_is_all_of(ctg, "LAScatalog")
  subset <- match.arg(subset)

  if (is_lascatalog_v3(ctg)) ctg <- lascatalog_v3_repair(ctg)

  i <- NULL

  if (is(y, "Extent") | inherits(y, "Raster"))
  {
    y <- sf::st_bbox(y)
    sf::st_crs(y) <- st_crs(ctg)
  }

  if (is(y, "stars"))
    y <- sf::st_bbox(y)

  if (is(y, "bbox"))
    y <- sf::st_as_sfc(y)

  if (is(y, "Spatial"))
    y <- sf::st_as_sf(y)

  if (is(y, "sf"))
    y <- sf::st_geometry(y)

  if (is(y, "sfc"))
  {
    sfctg <- ctg@data
    i <- if (is(y, "sfc_POINT")) sf::st_within(y, sfctg, sparse = T) else sf::st_intersects(y, sfctg, sparse = T)
    i <- Filter(function(x) {length(x) > 0}, i)
    i <- Reduce(c, i)
    i <- unique(i)
  }

  if (is.null(i))
    stop("Not supported input geometry", call. = FALSE)

  if (subset == "subset") {
    ctg <- ctg[i,]
  } else if (subset == "flag_unprocessed") {
    ctg$processed <- TRUE
    ctg$processed[i] <- FALSE
  } else {
    ctg$processed <- FALSE
    ctg$processed[i] <- TRUE
  }

  return(ctg)
}
