#' @param res numeric. The resolution of the output. Can optionally be a `RasterLayer` or a `stars` or
#' a `SpatRaster`. In that case the raster is used as the template.
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) meaning that the
#' grid aligns on (0,0). Not consiered if `res` is a raster
#' @rdname aggregate
#' @export
#' @md
pixel_metrics = function(las, func, res = 20, start = c(0,0), ...)
{
  UseMethod("pixel_metrics", las)
}

#' @export
pixel_metrics.LAS = function(las, func, res = 20, start = c(0,0), ...)
{
  # Defensive programming
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_numeric(start)

  template <- raster_layout(las, res, start)
  M <- template_metrics(las, func, template, ...)
  return(M)
}

#' @export
pixel_metrics.LAScluster = function(las, func, res = 20, start = c(0,0), ...)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)

  # crop the raster to the extent of the chunk because the raster can be a very large proxy
  # that will be materialized by rasterize_terrain LAS
  if (is_raster(res)) res <- raster_crop(res, st_adjust_bbox(x, raster_res(res)))

  M <- pixel_metrics(x, func, res, start, ...)
  M <- raster_crop(M, st_bbox(las))
  return(M)
}


#' @export
pixel_metrics.LAScatalog = function(las, func, res = 20, start = c(0,0), ...)
{
  if (is_a_number(res))
    assert_all_are_non_negative(res)
  else if (!raster_is_supported(res))
    stop("'res' must be a number of a raster.", call. = FALSE)

  assert_is_numeric(start)

  # Compute the alignment options including the case when res is a RasterLayer/stars/terra
  alignment <- raster_alignment(res, start)

  # subset the collection to the size of the layout (if any)
  if (!is_a_number(res)) las <- catalog_intersect(las, res)

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Enforce some options
  if (opt_wall_to_wall(las)) opt_chunk_buffer(las) <- 0.1*alignment[["res"]]

  # Processing
  globals = NULL
  if (engine_use_future()) globals <- future::getGlobalsAndPackages(func)
  options <- list(need_buffer = FALSE, drop_null = TRUE, globals = names(globals$globals), raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_apply(las, pixel_metrics, func = func, res = res, start = start, ..., .options = options)
  return(output)
}
