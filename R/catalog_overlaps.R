catalog_overlaps <- function(catalog)
{
  geom <- sf::st_geometry(st_as_sf(catalog))
  ii <- sf::st_intersects(geom)

  if (is_lascatalog_v3(catalog)) catalog <- lascatalog_v3_repair(catalog)

  intersections <- vector("list", length(ii))
  for (i in 1:length(ii))
  {
    k <- ii[[i]]
    k <- k[k != i]

    if (length(k) == 0) next

    p <- geom[k]
    q <- geom[i]
    w <- sf::st_intersection(q, p)

    intersections[[i]] <- w
  }

  intersections <- do.call(c, intersections)
  return(intersections)
}
