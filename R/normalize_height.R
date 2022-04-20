#' @export
#' @rdname normalize
normalize_height = function(las, algorithm, use_class = c(2L,9L), dtm = NULL, ...)
{
  UseMethod("normalize_height", las)
}

#' @export
normalize_height.LAS = function(las, algorithm, use_class = c(2L,9L), dtm = NULL, ...)
{
  # Non standard evaluation (R CMD check)
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  lidR.context <- "normalize_height"

  # Ellipsis parsing
  dots <- list(...)
  Wdegenerated <- isTRUE(dots$Wdegenerated)
  add_lasattribute <- isTRUE(dots$add_lasattribute)

  # If algorithm is raster we make a basic substraction
  if (is_raster(algorithm))
  {
    dtm <- algorithm

    if (raster_nlayer(dtm) > 1)
      stop("A DTM must be a single layer raster", call. = FALSE)

    Zg <- raster_value_from_xy(dtm, las$X, las$Y)
    isna <- is.na(Zg)
    nnas <- n_na_not_in_buffer(las, isna)

    # If it remains NAs (not from the buffer) it means that we have points that are not in the DTM
    # (the DTM does not cover the point cloud entirely). We will find a value anyway.
    # Previously the function stopped. It now forces interpolation with NN.
    if (nnas > 0)
    {
      nn <- knnidw(1, rmax = .Machine$double.xmax)
      znn <- nn(raster_as_las(dtm, bbox = st_bbox(las)), las@data[isna, .(X,Y,Z)])
      Zg[isna] <- znn
      warning(glue::glue("{nnas} points do not belong in the raster. Nearest neighbor was used to assign a value."), call. = FALSE)
    }
  }
  # If algorithm is spatial interpolation function
  else if (is.function(algorithm))
  {
    assert_is_algorithm(algorithm)
    assert_is_algorithm_spi(algorithm)

    if (any(as.integer(use_class) != use_class))
      stop("'use_class' is not a vector of integers'", call. = FALSE)

    use_class <- as.integer(use_class)

    if (!"Classification" %in% names(las))
      stop("No attribute 'Classification' found. This attribute is required to interpolate ground points.", call. = FALSE)

    # Select the ground points. If dtm is provided, then the ground point are from the DTM
    if (!is.null(dtm))
    {
      if (raster_nlayer(dtm) > 1)
        stop("A DTM must be a single layer raster", call. = FALSE)

      dtm <- raster_crop(dtm, st_bbox(las))
      ground <- raster_as_las(dtm, st_bbox(las))
    }
    else
    {
      ground  <- las@data[Classification %in% use_class, .(X,Y,Z)]
      if (nrow(ground) == 0) stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)
      ground  <- check_degenerated_points(ground, Wdegenerated)
    }

    # Interpolate the terrain providing what to interpolate (ground) and where
    # to interpolate (the point clouf)
    ground <- LAS(ground, las@header, crs = st_crs(las), check = FALSE, index = las@index)
    Zg <- algorithm(ground, las@data)
    Zg[is.nan(Zg)] <- NA_real_

    # If it remains NAs (not in buffer) it means that we have points very far from ground points
    # and they cannot be interpolated. But we will interpolate them anyway. Previously
    # the function stopped. It now forces interpolation with NN.
    isna <- is.na(Zg)
    nnas <- n_na_not_in_buffer(las, isna)
    if (nnas > 0)
    {
      nn <- knnidw(1, rmax = .Machine$double.xmax)
      znn <- nn(ground, las@data[isna, .(X,Y,Z)])
      Zg[isna] <- znn
      warning(glue::glue("Interpolation of {nnas} points failed because they are too far from ground points. Nearest neighbor was used but interpolation is weak for those points"), call. = FALSE)
    }
  }
  else
  {
    stop(glue::glue("Parameter 'algorithm' is a {class(algorithm)}. Expected type is 'raster' or 'function'"), call. = FALSE)
  }

  las[["Z offset"]] <- 0
  zoffset <- las[["Z offset"]]
  zscale <- las[["Z scale factor"]]

  if (!"Zref" %in% names(las))
    las@data[["Zref"]] <- las[["Z"]]

  las@data[["Z"]] <- las[["Z"]] - Zg

  if (add_lasattribute && is.null(las@header@VLR$Extra_Bytes[["Extra Bytes Description"]][["Zref"]]))
    las <- add_lasattribute_manual(las, name = "Zref", desc = "Elevation above sea level", type = "int", offset = zoffset, scale = zscale)

  fast_quantization(las@data[["Z"]], zscale, zoffset)
  las <- las_update(las)
  las@index$sensor <- las@index$sensor + NLAS
  return(las)
}

#' @export
normalize_height.LAScatalog = function(las, algorithm, use_class = c(2L,9L), dtm = NULL, ...)
{
  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, normalize_height, algorithm = algorithm, use_class = use_class, dtm = dtm, ..., .options = options)
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
#' @md
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

n_na_not_in_buffer <- function(las, isna)
{
  nnas <- sum(isna)
  if (nnas == 0) return(0)

  if (!"buffer" %in% names(las))
    return(nnas)

  buffer_na <- las$buffer[isna]
  return(sum(buffer_na == LIDRNOBUFFER))
}
