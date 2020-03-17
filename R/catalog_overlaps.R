catalog_overlaps <- function(catalog)
{
  spdf <- as.spatial(catalog)
  ii <- rgeos::gIntersects(spdf, byid = TRUE, returnDense = FALSE)

  intersections <- vector("list", length(ii))

  for (i in 1:length(ii))
  {
    k <- ii[[i]]
    k <- k[k != i]

    if (length(k) == 0) next

    p <- spdf[k, ]
    q <- spdf[i, ]
    w <- rgeos::gIntersection(q, p)

    if (is(w, "SpatialPoints")) next

    if (is(w, "SpatialLines")) next

    if (is(w, "SpatialCollections")) {
      if (is.null(w@polyobj)) next
      w <- w@polyobj
    }

    w@polygons[[1]]@ID <- as.character(i)
    intersections[[i]] <- w
  }

  if (all(sapply(intersections, is.null))) {
    empty <- sp::SpatialPolygons(list())
    raster::projection(empty) <- raster::projection(catalog)
    return(empty)
  }

  output <- do.call(rbind, intersections)
  output <- rgeos::gUnaryUnion(output)
  return(output)
}
