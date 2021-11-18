#' @export
#' @rdname segment
segment_shapes = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  UseMethod("segment_shapes", las)
}

#' @export
segment_shapes.LAS = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  stopif_forbidden_name(attribute)
  assert_is_a_string(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_shp(algorithm)
  lidR.context <- "segment_shapes"

  filter    <- parse_filter(las, filter)
  output    <- algorithm(las, filter)
  las@data[[attribute]] <- output
  return(las)
}

#' @export
segment_shapes.LAScatalog = function(las, algorithm, attribute = "Shape", filter = NULL)
{
  stopif_forbidden_name(attribute)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_shp(algorithm)

  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, segment_shapes,  algorithm = algorithm, attribute = attribute, filter = filter, .options = options)
  return(output)
}
