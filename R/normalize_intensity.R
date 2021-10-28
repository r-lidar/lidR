#' @export
#' @rdname normalize
normalize_intensity <- function(las, algorithm)
{
  UseMethod("normalize_intensity", las)
}

#' @export
normalize_intensity.LAS <- function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_nit(algorithm)
  lidR.context <- "normalize_intensity"
  intensity <- algorithm(las)
  invalid   <- fast_countequal(intensity, 65535)

  if (invalid > 0)
    warning(glue::glue("{invalid} points have a normalized intensity greater than 65535. Intensity replaced by 65535"), call. = FALSE)

  las@data[["RawIntensity"]] <- las@data[["Intensity"]]
  las@data[["Intensity"]]    <- intensity

  return(las)
}

#' @export
normalize_intensity.LAScatalog = function(las, algorithm)
{
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, normalize_intensity, algorithm = algorithm, .options = options)
  return(output)
}
