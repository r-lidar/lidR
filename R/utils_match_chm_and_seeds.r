match_chm_and_seeds = function(chm, seeds, field)
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_all_of(seeds, "SpatialPointsDataFrame")
  stopif_forbidden_name(field)

  if(is.null(raster::intersect(raster::extent(chm), raster::extent(seeds))))
    stop("No seed matches with the canopy height model" )

  if (field %in% names(seeds@data))
  {
    ids = seeds@data[[field]]

    if (!is.numeric(ids))
      stop("Tree IDs much be of numeric type.",  call. = FALSE)

    if (length(unique(ids)) < length(ids))
      stop("Duplicated tree IDs found.")
  }
  else
    ids = 1:nrow(seeds@data)

  cells = raster::cellFromXY(chm, seeds)

  if (anyNA(cells))
  {
    if (all(is.na(cells)))
      stop("No seed found")
    else
      warning("Some seeds are outside the canopy height model. They were removed.")

    no_na = !is.na(cells)
    seeds = seeds[no_na,]
    cells = cells[no_na]
  }

  return(list(cells = cells, ids = ids))
}