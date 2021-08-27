catalog_overlaps <- function(catalog)
{
  sfdf <- sf::st_geometry(sf::st_as_sf(catalog))
  ii <- sf::st_intersects(sfdf)

  intersections <- vector("list", length(ii))
  for (i in 1:length(ii))
  {
    k <- ii[[i]]
    k <- k[k != i]

    if (length(k) == 0) next

    p <- sfdf[k]
    q <- sfdf[i]
    w <- sf::st_intersection(q, p)

    intersections[[i]] <- w
  }

  intersections <- do.call(c, intersections)
  sf::as_Spatial(intersections)
}
