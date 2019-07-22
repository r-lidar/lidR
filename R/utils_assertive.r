assert_all_are_non_negative = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x >= 0))
    stop(glue::glue("Values of {x.} are not all positive or null."), call. = FALSE)
}


assert_all_are_positive = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x > 0))
    stop(glue::glue("Values of {x.} are not all positive."), call. = FALSE)
}


assert_all_are_in_closed_range = function(x, a, b)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x >= a) | !all(x <= b))
    stop(glue::glue("Values of {x.} are not all in range [{a}, {b}]."), call. = FALSE)
}


assert_all_are_in_open_range = function(x, a, b)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x > a) | !all(x < b))
    stop(glue::glue("Values of {x.} are not all in range ]{a}, {b}[."), call. = FALSE)
}

assert_all_are_true = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x == TRUE))
    stop(glue::glue("Values of {x.} are not all TRUE."), call. = FALSE)
}

assert_are_same_length = function(x, y)
{
  x. <- lazyeval::expr_text(x)
  y. <- lazyeval::expr_text(y)
  if (length(x) != length(y))
    stop(glue::glue("{x.} and {y.} have different lengths."), call. = FALSE)
}

assert_all_are_existing_files = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(file.exists(x)))
    stop(glue::glue("File does not exist."), call. = FALSE)
}

assert_is_a_bool = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.logical(x) | length(x) > 1)
    stop(glue::glue("{x.} is not a boolean."), call. = FALSE)
}

assert_is_a_number = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.numeric(x) | length(x) > 1)
    stop(glue::glue("{x.} is not a number."), call. = FALSE)
}

is_a_number = function(x)
{
  return(is.numeric(x) & length(x) == 1)
}

assert_is_a_string = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.character(x) | length(x) > 1)
    stop(glue::glue("{x.} is not a string."), call. = FALSE)
}


assert_is_numeric = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.numeric(x))
    stop(glue::glue("{x.} is not numeric it is a {class(x)}."), call. = FALSE)
}

assert_is_vector = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.vector(x))
    stop(glue::glue("{x.} is not a vector it is a {class(x)}."), call. = FALSE)
}

assert_is_list = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.list(x))
    stop(glue::glue("{x.} is not a list it is a {class(x)}"), call. = FALSE)
}

assert_is_of_length = function(x, l)
{
  x. <- lazyeval::expr_text(x)
  if (length(x) != l)
    stop(glue::glue("{x.} is not of length {l} it is of length {length(x)}."), call. = FALSE)
}

assert_is_character = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.character(x))
    stop(glue::glue("{x.} is not character it is a {class(x)}"), call. = FALSE)
}


assert_is_all_of = function(x, class)
{
  x. <- lazyeval::expr_text(x)
  if (!is(x, class))
    stop(glue::glue("{x.} is not a {class} it is {class(x)}."), call. = FALSE)
}

assert_is_function = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!is.function(x))
    stop(glue::glue("{x.} is not a function it is a {class(x)}."), call. = FALSE)
}

assert_all_are_same_crs = function(x)
{
  if (!sp::identicalCRS(x))
    stop("Different CRS.", call. = FALSE)
}

assert_is_algorithm = function(x)
{
  if (!is.algorithm(x))
    stop("Invalid function provided as algorithm.", call. = FALSE)
}

assert_is_algorithm_dsm = function(x)
{
  if (!is(x, "DigitalSurfaceModel"))
    stop("The algorithm used is not an algorithm for digital surface model.", call. = FALSE)
}

assert_is_algorithm_itd = function(x)
{
  if (!is(x, "IndividualTreeDetection"))
    stop("The algorithm used is not an algorithm for individual tree detection.", call. = FALSE)
}

assert_is_algorithm_shp = function(x)
{
  if (!is(x, "ShapeDetection"))
    stop("The algorithm used is not an algorithm for shape detection.", call. = FALSE)
}

assert_is_algorithm_spi = function(x)
{
  if (!is(x, "SpatialInterpolation"))
    stop("The algorithm used is not an algorithm for spatial interpolation.", call. = FALSE)
}

assert_is_algorithm_its = function(x)
{
  if (!is(x, "IndividualTreeSegmentation"))
    stop("The algorithm used is not an algorithm for individual tree segmentation.", call. = FALSE)
}

assert_is_algorithm_dec = function(x)
{
  if (!is(x, "PointCloudDecimation"))
    stop("The algorithm used is not an algorithm for point cloud decimation.", call. = FALSE)
}

assert_is_algorithm_gnd = function(x)
{
  if (!is(x, "GroundSegmentation"))
    stop("The algorithm used is not an algorithm for ground segmentation.", call. = FALSE)
}

assert_is_algorithm_sng = function(x)
{
  if (!is(x, "SnagsSegmentation"))
    stop("The algorithm used is not an algorithm for snags segmentation.", call. = FALSE)
}


assert_is_valid_context = function(expected_contexts, name = "", null_allowed = FALSE)
{
  received_context <- tryCatch({get("lidR.context", envir = parent.frame(n = 2L))}, error = function(e) {return(NULL)})

  if (is.null(received_context) && !null_allowed)
    stop(glue::glue("The '{name}' algorithm has not been called in the correct context. Maybe it has been called alone but it should be used within a lidR function."), call. = FALSE)
  else
    return(NULL)

  if (!received_context %in% expected_contexts)
    stop(glue::glue("The '{name}' algorithm has not been called in the correct context."), call. = FALSE)

  return(NULL)
}


stopifnotlas = function(x)
{
  if (!inherits(x, "LAS"))
    stop("Argument is not a LAS object", call. = FALSE)
}

stopif_forbidden_name = function(name)
{
  if (name %in% LASFIELDS)
    stop(glue::glue("{name} is part of the core attributes and is a forbidden name."), call. = FALSE)
}
