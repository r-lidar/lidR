#' @export
#' @rdname normalize
normalize_height = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  UseMethod("normalize_height", las)
}

#' @export
normalize_height.LAS = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  assert_is_a_bool(na.rm)
  assert_is_a_bool(add_lasattribute)
  assert_is_a_bool(Wdegenerated)

  if (is(algorithm, "stars_proxy"))
    stop("stars_proxy not supported yet")

  if (is_raster(algorithm))
  {
    Zground <- raster_value_from_xy(algorithm, las$X, las$Y)
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if (nnas > 0 && na.rm == FALSE)
      stop(glue::glue("{nnas} points were not normalizable because the DTM contained NA values. Process aborted."))
  }
  else if (is.function(algorithm))
  {
    assert_is_algorithm(algorithm)
    assert_is_algorithm_spi(algorithm)

    if (any(as.integer(use_class) != use_class))
      stop("'add_class' is not a vector of integers'", call. = FALSE)

    use_class <- as.integer(use_class)

    if (!"Classification" %in% names(las))
      stop("No field 'Classification' found. This attribute is required to interpolate ground points.", call. = FALSE)

    # Non standard evaluation (R CMD check)
    . <- Z <- Zref <- X <- Y <- Classification <- NULL

    # Select the ground points
    ground  <- las@data[Classification %in% c(use_class), .(X,Y,Z)]
    if (nrow(ground) == 0) stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)
    ground  <- check_degenerated_points(ground, Wdegenerated)

    # wbuffer = !"buffer" %in% names(las)
    lidR.context <- "normalize_height"
    ground  <- LAS(ground, las@header, crs = st_crs(las), check = FALSE, index = las@index)
    Zground <- algorithm(ground, las@data)
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if (nnas > 0 & na.rm == FALSE)
      stop(glue::glue("{nnas} points were not normalizable. Process aborted."), call. = FALSE)
  }
  else
  {
    stop(glue::glue("Parameter 'algorithm' is a {class(algorithm)}. Expected type is 'raster' or 'function'"), call. = FALSE)
  }

  zoffset <- las@header@PHB[["Z offset"]]
  zscale <- las@header@PHB[["Z scale factor"]]

  if (!"Zref" %in% names(las))
    las@data[["Zref"]] <- las@data[["Z"]]

  las@data[["Z"]] <- las@data[["Z"]] - Zground

  if (add_lasattribute && is.null(las@header@VLR$Extra_Bytes[["Extra Bytes Description"]][["Zref"]]))
    las <- add_lasattribute_manual(las, name = "Zref", desc = "Elevation above sea level", type = "int", offset = zoffset, scale = zscale)

  if (nnas > 0 && na.rm == TRUE)
  {
    las <- las[!isna]
    message(glue::glue("{nnas} points were not normalizable and removed."))
  }

  fast_quantization(las@data[["Z"]], zscale, zoffset)
  las <- las_update(las)
  las@index$sensor <- las@index$sensor + NLAS
  return(las)
}

#' @export
normalize_height.LAScatalog = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, normalize_height, algorithm = algorithm, na.rm = na.rm, use_class = use_class, ..., add_lasattribute = add_lasattribute, Wdegenerated = Wdegenerated, .options = options)
  return(output)
}

#' @rdname normalize
#' @export
unnormalize_height = function(las)
{
  stopifnotlas(las)

  if ("Zref" %in% names(las))
  {
    las@data[["Z"]] <- las@data[["Zref"]]
    las@data[["Zref"]] <- NULL
    las <- lasupdateheader(las)
  }
  else
    message("No attribute 'Zref' found. Un-normalizisation is impossible.")

  if (las@index$sensor > NLAS) las@index$sensor <- las@index$sensor - NLAS
  return(las)
}

#' @param e1 a LAS object
#' @param e2 A raster representing a digital terrain model in format from `raster`, `stars` or `terra`..
#' @export
#' @rdname normalize
setMethod("-", c("LAS"), function(e1, e2)
{
  return(normalize_height(e1,e2))
})

check_degenerated_points = function(points, Wdegenerated = TRUE)
{
  . <- X <- Y <- Z <- NULL

  # test integrity of the data and degenerated points
  dup_xyz  = duplicated(points, by = c("X", "Y", "Z"))
  dup_xy   = duplicated(points, by = c("X", "Y"))
  ndup_xyz = sum(dup_xyz)
  ndup_xy  = sum(dup_xy & !dup_xyz)

  if (ndup_xyz > 0 && Wdegenerated)
    warning(glue::glue("There were {ndup_xyz} degenerated ground points. Some X Y Z coordinates were repeated. They were removed."), call. = FALSE)

  if (ndup_xy > 0 && Wdegenerated)
    warning(glue::glue("There were {ndup_xy} degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates. min Z were retained."), call. = FALSE)

  if (ndup_xy > 0 | ndup_xyz > 0)
    points = points[, .(Z = min(Z)), by = .(X,Y)]

  return(points)
}
