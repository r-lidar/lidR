#' @export
#' @rdname classify
classify_ground = function(las, algorithm, last_returns = TRUE)
{
  UseMethod("classify_ground", las)
}

#' @export
classify_ground.LAS = function(las, algorithm, last_returns = TRUE)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_gnd(algorithm)

  filter <- TRUE
  if (last_returns) {
    if (!all(c("ReturnNumber", "NumberOfReturns") %in% names(las))) {
      warning("'ReturnNumber' and/or 'NumberOfReturns' not found. Cannot use the option 'last_returns', all the points will be used.", call. = FALSE)
    } else {
      filter <- parse_filter(las, ~ReturnNumber == NumberOfReturns)
      if (sum(filter) == 0) {
        warning("Zero last return found. Cannot use the option 'last_returns', all the points will be used.", call. = FALSE)
        filter <- TRUE
      }
    }
  }

  lidR.context <- "classify_ground"
  idx <- algorithm(las, filter)

  if ("Classification" %in% names(las))
  {
    nground <- fast_countequal(las@data[["Classification"]], 2L)

    if (nground > 0)
    {
      message(glue::glue("Original dataset already contains {nground} ground points. These points were reclassified as 'unclassified' before performing a new ground classification."))
      new_classes <- las@data[["Classification"]]
      new_classes[new_classes == LASGROUND] <- LASUNCLASSIFIED
    }
    else
    {
      new_classes <- las@data[["Classification"]]
    }
  }
  else
    new_classes <- rep(LASUNCLASSIFIED, npoints(las))

  new_classes[idx] <- LASGROUND
  las@data[["Classification"]] <- new_classes
  return(las)
}

#' @export
classify_ground.LAScatalog = function(las, algorithm, last_returns = TRUE)
{
  opt_select(las) <- "*"
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, classify_ground, algorithm = algorithm,  last_returns = last_returns, .options = options)
  return(output)
}
