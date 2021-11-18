point_in_polygons <- function(las, sfc)
{
  if (!is(sfc, "sfc_POLYGON") & !is(sfc, "sfc_MULTIPOLYGON"))
    stop("Only spatial polygons are supported", call. = FALSE)

  bbox <- sf::st_as_sfc(st_bbox(las))
  id  <- unlist(sf::st_intersects(bbox, sfc))
  id  <- as.integer(id)
  sfc2 <- sfc[id]

  wkt <- sf::st_as_text(sfc2, digit = 10)
  res <- C_in_polygon(las, wkt)
  idx <- id[res+1L]
  return(idx)
}
