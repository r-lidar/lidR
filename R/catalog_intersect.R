#' @export
#' @rdname catalog_subset
catalog_intersect = function(ctg, y, ..., subset = c("subset", "flag_unprocessed", "flag_processed"))
{
  assert_is_all_of(ctg, "LAScatalog")
  subset <- match.arg(subset)

  if (is_lascatalog_v3(ctg)) ctg <- lascatalog_v3_repair(ctg)

  i <- NULL

  if (is(y, "Extent"))
  {
    y <- sf::st_bbox(y)
    sf::st_crs(y) <- st_crs(ctg)
  }

  if (is_raster(y))
    y <- raster_bbox(y)

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
