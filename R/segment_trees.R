#' @export
#' @rdname segment
segment_trees = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  UseMethod("segment_trees", las)
}

#' @export
segment_trees.LAS = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  stopif_forbidden_name(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_its(algorithm)
  match.arg(uniqueness, c('incremental', 'gpstime', 'bitmerge'))
  lidR.context <- "segment_trees"

  if (uniqueness == 'gpstime' && !"gpstime" %in% names(las))
    stop("Impossible to compute unique IDs using gpstime: no gpstime found.", call. = FALSE)

  if (uniqueness == 'gpstime' &&  fast_countequal(las[["gpstime"]], 0L) == npoints(las))
    stop("Impossible to compute unique IDs using gpstime: gpstime is not populated.", call. = FALSE)

  if (is(algorithm, "RasterBased"))
    output <- algorithm(st_bbox(las))
  else if (is(algorithm, "PointCloudBased"))
    output <- algorithm(las)
  else
    stop("Invalid algorithm provided in segment_trees. The algorithm must have a class 'RasterBased' or 'PointCloudBased'")

  if (is_raster(output))
    las <- merge_spatial(las, output, attribute)
  else if (is.integer(output))
    las <- add_attribute(las, output, attribute)
  else
    stop(glue::glue("Wrong output type for the algorithm used. Expected 'RasterLayer' or 'integer', received {class(output)}"))

  if (all(is.na(las@data[[attribute]])))
    message("No tree found. Maybe use different parameters.")

  # If uniqueness is incremental we are done
  if (uniqueness == 'incremental')
  {
    type <- if (is.integer(las[[attribute]])) "int" else "double"
    NA_value <- if (is.integer(las[[attribute]])) .Machine$integer.max else .Machine$double.xmin
    las <- add_lasattribute_manual(las, name = attribute, desc = "An ID for each segmented tree", type = type, NA_value = NA_value)
    return(las)
  }

  # Otherwise we must compute an ID that is guaranteed to be unique

  tapex <- function(z,t) {
    zmax <- max(z)
    j <- which(z == zmax) # allows to retrieve potentially multiple points that are all max
    if (length(j) > 1) j <- j[which.min(t[j])] # arbitrarily takes the lowest gpstime
    return(list(t.pos.t = t[j]))
  }

  xyapex <- function(x,y,z) {
    zmax <- max(z)
    j <- which(z == zmax) # allows to retrieve potentially multiple points that are all max
    if (length(j) > 1) j <- j[which.min(x[j])] # arbitrarily takes the lowest x coordinate
    return(list(x.pos.t = x[j], y.pos.t = y[j]))
  }

  X <- Y <- Z <- gpstime <- NULL
  ids <- las@data[[attribute]]

  if (uniqueness == 'gpstime')
  {
    identifyers <- las@data[, if (!is.na(.BY)) tapex(Z, gpstime), by = ids]

    ids <- data.frame(ids)
    data.table::setDT(ids)
    matching <- identifyers[ids, on = 'ids']

    las@data[[attribute]] <- matching[["t.pos.t"]]

    las <- add_lasattribute_manual(las, name = attribute, desc = "An ID for each segmented tree", type = "double", NA_value = .Machine$double.xmin)
    return(las)
  }

  if (uniqueness == 'bitmerge')
  {
    identifyers <- las@data[, if (!is.na(.BY)) xyapex(X, Y, Z), by = ids]

    xoffset <- las[["X offset"]]
    yoffset <- las[["Y offset"]]
    zoffset <- las[["Z offset"]]

    xscale  <- las[["X scale factor"]]
    yscale  <- las[["Y scale factor"]]
    zscale  <- las[["Z scale factor"]]

    xscaled <- as.integer((identifyers[["x.pos.t"]] - xoffset)/xscale)
    yscaled <- as.integer((identifyers[["y.pos.t"]] - yoffset)/yscale)

    identifyers[["IDs"]] <- bitmerge(xscaled, yscaled)

    identifyers <- identifyers[, c(1,4)]
    ids <- data.frame(ids)
    data.table::setDT(ids)
    matching <- identifyers[ids, on = 'ids']

    las@data[[attribute]] <- matching[["IDs"]]

    las <- add_lasattribute_manual(las, name = attribute, desc = "An ID for each segmented tree", type = "double", NA_value = .Machine$double.xmin)
    return(las)
  }
}

#' @export
segment_trees.LAScatalog = function(las, algorithm, attribute = "treeID", uniqueness = 'incremental')
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_its(algorithm)

  # Enforce some options
  opt_select(las) <- "*"

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, segment_trees, algorithm = algorithm, attribute = attribute, uniqueness = uniqueness, .options = options)
  return(output)
}
