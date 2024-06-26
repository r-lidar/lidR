point_in_polygons <- function(las, sfc, by_poly = FALSE)
{
  if (!all(sf::st_geometry_type(sfc) %in% c("POLYGON", "MULTIPOLYGON")))
    stop("Only spatial polygons are supported", call. = FALSE)

  bbox <- sf::st_as_sfc(st_bbox(las))
  id  <- unlist(sf::st_intersects(bbox, sfc))
  id  <- as.integer(id)
  sfc2 <- sfc[id]

  rings = extract_polygons(sfc2)

  res <- C_in_polygon(las, rings, by_poly)
  return(res)
}

extract_polygons = function(sfc)
{
  # Fix #763. Complex code to avoid using boost
  coordinates = lapply(sfc, sf::st_coordinates)
  rings = lapply(coordinates, extract_rings)
  return(rings)
}

extract_rings = function(coordinates)
{
  if (ncol(coordinates) == 5L)
    features = lapply( split( coordinates[,1:5], coordinates[,5] ), matrix, ncol = 5)
  else
    features = list(coordinates)

  polygons = lapply(features,  function (x) lapply( split( x[,1:3], x[,4] ), matrix, ncol = 3))
  rings = lapply(polygons,  function (x) lapply(x, function(x) lapply( split( x[,1:3], x[,3] ), matrix, ncol = 3)))
  rings = lapply(rings, function(x) Reduce(c, x))

  return(rings[[1]])
}
