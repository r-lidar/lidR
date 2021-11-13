#' @export
#' @rdname rasterize
rasterize_terrain = function(las, res = 1, algorithm = tin(), use_class = c(2L,9L), shape = "convex", ...)
{
  UseMethod("rasterize_terrain", las)
}

#' @export
rasterize_terrain.LAS = function(las, res = 1, algorithm = tin(), use_class = c(2L,9L), shape = "convex", ...)
{
  # NOTE: the complexity of the code comes from the fact that res can be a number, a RasterLayer
  # a stars or a SpatRaster, and the function must return a corresponding raster. This implies
  # to use some raster agnostic wrappers

  # Defensive programming
  if (is_a_number(res))
    assert_all_are_non_negative(res)
  else if (!raster_is_supported(res))
    stop("'res' must be a number or a raster.", call. = FALSE)

  assert_is_algorithm(algorithm)
  assert_is_algorithm_spi(algorithm)

  if (is.character(shape))
    shape <- match.arg(shape, c("convex", "concave", "bbox"))
  else if (!is(shape, "sfc_POLYGON") & !is(shape, "sfc_MULTIPOLYGON"))
    stop("Argument 'shape' must be a string or a sfc", call. = FALSE)

  if (!"Classification" %in% names(las)) stop("LAS object does not contain 'Classification' attribute", call. = FALSE)
  if (any(as.integer(use_class) != use_class)) stop("'use_class' is not a vector of integers'", call. = FALSE)
  use_class <- as.integer(use_class)

  # Ellipsis parsing
  dots <- list(...)
  Wdegenerated <- isTRUE(dots$Wdegenerated)
  keep_lowest <- isTRUE(dots$keep_lowest)
  pkg <- if (is.null(dots$pkg)) getOption("lidR.raster.default") else dots$pkg

  # Non standard evaluation (R CMD check)
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  # Select the ground points
  ground <- las@data[Classification %in% c(use_class), .(X,Y,Z)]
  if (nrow(ground) == 0) stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)
  ground <- check_degenerated_points(ground, Wdegenerated)

  # Find where to interpolate the DTM. Generate the XY where we want a DTM
  # It creates a stars object no matter the input which might be a number
  # a Raster, a stars or a SpatRaster
  layout <- if (!is_a_number(res)) raster_template(res) else raster_layout(las, res, format = "template")
  layout <- raster_materialize(layout, values = 0, pkg = "stars")

  if (is.character(shape) && shape %in% c("convex", "concave"))
  {
    if (shape == "concave")
      hull <- st_concave_hull(las, concavity = 10, length_threshold = 100)
    else
      hull <- st_convex_hull(las)

    hull <- sf::st_buffer(hull, dist = raster_res(layout)[1])
    layout <- layout[hull]
  }

  if (is(shape, "sfc"))
    layout <- layout[shape]

  grid <- raster_as_dataframe(layout, xy = TRUE, na.rm = TRUE)

  # Interpolate the terrain providing what to interpolate (ground) and where
  # to interpolate (grid)
  lidR.context <- "rasterize_terrain"
  ground <- LAS(ground, las@header, crs = st_crs(las), check = FALSE, index = las@index)
  Zg <- algorithm(ground, grid)
  Zg[is.nan(Zg)] <- NA_real_

  # If it remains NAs it means that we have points very far from ground points
  # and they cannot be interpolated. But we will interpolate them anyway. Previously
  # the function stopped. It now forces interpolation with NN.
  isna <- is.na(Zg)
  nnas <- sum(isna)
  if (nnas > 0)
  {
    nn <- knnidw(1, rmax = .Machine$double.xmax)
    sub_grid <- data.frame(X = grid$X[isna],  Y = grid$Y[isna])
    znn <- nn(ground, sub_grid)
    Zg[isna] <- znn
    warning(glue::glue("Interpolation of {nnas} points failed because they are too far from ground points. Nearest neighbour was used but interpolation is weak for those points"), call. = FALSE)
  }

  # Quantize the elevation for not returning absurdly accurate elevation
  fast_quantization(Zg, las[["Z scale factor"]], las[["Z offset"]])

  # Assignment of the values to a raster. It might be a raster from raster/stars/terra
  # This part of the code is raster agnostic. It returns a raster from the default format
  # registered in lidR or from pkg argument or the format given in 'res'
  cells  <- get_group(layout, grid)
  layout <- raster_layout(las, res)
  layout <- raster_materialize(layout, pkg = pkg)
  layout <- raster_set_values(layout, Zg, cells = cells)

  # Replace the interpolated value by the lowest point (legacy code) for option
  # keep_lowest that has been removed
  if (keep_lowest)
  {
    res    <- raster_res(layout)[1]
    lasg   <- filter_poi(las, Classification %in% c(use_class))
    rmin   <- template_metrics(lasg, ~list(Z = min(Z)), layout)
    Z1     <- raster_values(layout)
    Z2     <- raster_values(rmin)
    Zg     <- pmin(Z1, Z2, na.rm = TRUE)
    layout <- raster_set_values(layout, Zg)
  }

  raster_names(layout) <- "Z"
  return(layout)
}

#' @export
rasterize_terrain.LAScatalog = function(las, res = 1, algorithm = tin(), use_class = c(2L,9L), shape = "convex", ...)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_spi(algorithm)

  if (is_a_number(res))
    assert_all_are_non_negative(res)
  else if (!raster_is_supported(res))
    stop("'res' must be a number or a raster.", call. = FALSE)

  # subset the collection to the size of the layout (if any)
  if (!is_a_number(res)) las <- catalog_intersect(las, res)

  # Enforce some options
  opt_select(las) <- "xyzc"
  opt_filter(las) <-  paste("-keep_class", paste(use_class, collapse = " "), opt_filter(las))

  # Compute the alignment options including the case when res is a raster/stars/terra
  alignment <- raster_alignment(res)

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_map(las, rasterize_terrain, res = res, algorithm = algorithm, shape = shape, use_class = use_class, ..., .options = options)
  return(output)
}