round_any <- function(x, accuracy, f = round)
{
  f(x / accuracy) * accuracy
}