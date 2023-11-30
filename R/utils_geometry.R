point_in_polygons <- function(las, sfc, by_poly = FALSE)
{
  if (!all(sf::st_geometry_type(sfc) %in% c("POLYGON", "MULTIPOLYGON")))
    stop("Only spatial polygons are supported", call. = FALSE)

  bbox <- sf::st_as_sfc(st_bbox(las))
  id  <- unlist(sf::st_intersects(bbox, sfc))
  id  <- as.integer(id)
  sfc2 <- sfc[id]

  wkt <- sf::st_as_text(sfc2, digit = 10)
  res <- C_in_polygon(las, wkt, by_poly)
  return(res)
}
