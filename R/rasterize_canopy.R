#' @export
#' @rdname rasterize
rasterize_canopy = function(las, res = 1, algorithm = p2r(), ...)
{
  UseMethod("rasterize_canopy", las)
}

#' @export
rasterize_canopy.LAS = function(las, res = 1, algorithm = p2r(), ...)
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dsm(algorithm)
  assert_las_is_not_empty(las)

  dots <- list(...)
  pkg <- if (is.null(dots$pkg)) getOption("lidR.raster.default") else dots$pkg

  # Compute the raster that encompass the point cloud
  layout <- if (!is_a_number(res)) raster_template(res) else raster_layout(las, res, format = "template")
  layout <- raster_materialize(layout, pkg = "stars")

  # Compute the elevation for each cells
  lidR.context <- "rasterize_canopy"
  z <- algorithm(las, layout)

  # Quantize
  z <- round(z, 3)

  layout <- raster_layout(las, res)
  layout <- raster_materialize(layout, pkg = pkg)
  layout <- raster_set_values(layout, z)
  raster_names(layout) <- "Z"
  return(layout)
}

#' @export
rasterize_canopy.LAScluster = function(las, res = 1, algorithm = p2r(), ...)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)

  # crop the raster to the extent of the chunk because the raster can be a very large proxy
  # that will be materialized by rasterize_terrain LAS
  if (is_raster(res)) res <- raster_crop(res, st_adjust_bbox(x, raster_res(res)))

  chm <- rasterize_canopy(x, res, algorithm, ...)
  chm <- raster_crop(chm, st_bbox(las))
  return(chm)
}

#' @export
rasterize_canopy.LAScatalog = function(las, res = 1, algorithm = p2r(), ...)
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dsm(algorithm)

  if (is_a_number(res))
    assert_all_are_non_negative(res)
  else if (!raster_is_supported(res))
    stop("'res' must be a number of a raster.", call. = FALSE)

  # Compute the alignment options including the case when res is a RasterLayer/stars/terra
  alignment <- raster_alignment(res)

  # subset the collection to the size of the layout (if any)
  if (!is_a_number(res)) las <- catalog_intersect(las, res)

  # Enforce some options
  opt_select(las) <- "xyzr"
  if (opt_wall_to_wall(las)) opt_chunk_buffer(las) <- 2


  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_apply(las, rasterize_canopy, res = res, algorithm = algorithm, ..., .options = options)
  return(output)
}
