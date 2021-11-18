#' @export
#' @rdname classify
classify_noise = function(las, algorithm)
{
  UseMethod("classify_noise", las)
}

#' @export
classify_noise.LAS = function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_out(algorithm)

  lidR.context <- "classify_noise"
  idx <- algorithm(las)

  if ("Classification" %in% names(las))
  {
    nnoise <- fast_countequal(las@data[["Classification"]], LASNOISE)

    if (nnoise > 0)
    {
      message(glue::glue("Original dataset already contains {nnoise} noise points. These points were reclassified as 'unclassified' before performing a new noise classification."))
      new_classes <- las@data[["Classification"]]
      new_classes[new_classes == LASNOISE] <- LASUNCLASSIFIED
    }
    else
    {
      new_classes <- las@data[["Classification"]]
    }
  }
  else
    new_classes <- rep(LASUNCLASSIFIED, npoints(las))

  new_classes[idx] <- LASNOISE
  las@data[["Classification"]] <- new_classes
  return(las)
}

#' @export
classify_noise.LAScatalog = function(las, algorithm)
{
  opt_select(las) <- "*"
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, classify_noise, algorithm = algorithm, .options = options)
  return(output)
}
