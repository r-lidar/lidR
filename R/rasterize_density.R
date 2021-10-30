#' @export
#' @rdname rasterize
rasterize_density = function(las, res = 4, ...)
{
  UseMethod("rasterize_density", las)
}

#' @export
rasterize_density.LAS = function(las, res = 4, ...)
{
  dots <- list(...)
  pkg <- if (is.null(dots$pkg)) getOption("lidR.raster.default") else dots$pkg

  X <- raster_layout(las, res)
  X <- raster_materialize(X, pkg = pkg)

  if (!"pulseID" %in% names(las))
  {
    template <- if (!is_a_number(res)) raster_template(res) else raster_layout(las, res, format = "template")
    count  <- fasterize(las, template, FALSE, "count")

    X <- raster_set_values(X, count)
    raster_names(X) <- "density"
  }
  else
  {
    X <- pixel_metrics(las, ~list(point_density = .N, pulse_density = length(unique(pulseID))), X)
  }

  X <- raster_replace_na(X)
  X <- X/(raster_res(X)[1]^2)
  return(X)
}

#' @export
rasterize_density.LAScatalog = function(las, res = 4, ...)
{
  # Defensive programming
  if (is_a_number(res))
    assert_all_are_non_negative(res)
  else if (!raster_is_supported(res))
    stop("'res' must be a number of a raster.", call. = FALSE)

  # Compute the alignment options including the case when res is a RasterLayer/stars/terra
  alignment <- raster_alignment(res)

  # subset the collection to the size of the layout (if any)
  if (!is_a_number(res)) las <- catalog_intersect(las, res)

  # Enforce some options
  opt_select(las) <- "xyz"
  if (opt_wall_to_wall(las))
    opt_chunk_buffer(las) <- 2

  # Compute the alignment option including the case when res is a RasterLayer
  alignment   <- list(res = res, start = c(0,0))

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, raster_alignment = alignment)
  output  <- catalog_map(las, rasterize_density, res = res, ..., .options = options)
  return(output)
}
