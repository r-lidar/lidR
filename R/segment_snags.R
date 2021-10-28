#' @export
#' @rdname segment
segment_snags = function(las, algorithm, attribute = "snagCls")
{
  UseMethod("segment_snags", las)
}

#' @export
segment_snags.LAS = function(las, algorithm, attribute = "snagCls")
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_sng(algorithm)
  stopif_forbidden_name(attribute)

  lidR.context <- "segment_snags"
  snags <- algorithm(las)

  las <- add_lasattribute(las, snags, attribute, "Number identifying a snag class")
  return(las)
}

#' @export
segment_snags.LAScatalog = function(las, algorithm, attribute = "snagCls")
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_sng(algorithm)
  stopif_forbidden_name(attribute)

  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, segment_snags,  algorithm = algorithm, .options = options)
  return(output)
}
