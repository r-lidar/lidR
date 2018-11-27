# All that stuff aims to replace the functions needed by lidR that come from the assertive package.
# On 2018-11-20 I received an email from Kurt Hornik to announce that assertive will be removed
# from cran and consequently lidR as well because it as a strong dependency to assertive. This file
# is quick fix that removes dependency to assertive.

assert_all_are_non_negative = function(x)
{
  x. <- lazyeval::expr_text(x)
  if (!all(x >= 0))
    stop(glue::glue("Values of {x.} are not all positive or nul.."), call. = FALSE)
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
    stop(glue::glue("Values of {x.} are not all existing files."), call. = FALSE)
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
    stop(glue::glue("{x.} is not vector it is a {class(x)}."), call. = FALSE)
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
    stop(glue::glue("{x.} is not a function it is a {class(x)}"), call. = FALSE)
}
