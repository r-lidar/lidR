round_any <- function(x, accuracy)
{
  roundc(x / accuracy) * accuracy
}

group_grid_3d = function(x, y, z, res, start = c(0,0,0))
{
  xgrid = f_grid(x, res[1], start[1])
  ygrid = f_grid(y, res[1], start[2])
  zgrid = f_grid(z, res[2], start[3])

  return(list(Xgrid = xgrid, Ygrid = ygrid, Zgrid = zgrid))
}

f_grid = function(x, res, start)
{
  round_any(x - 0.5 * res - start, res) + 0.5 * res + start
}

verbose = function(...)
{
  if (getOption("lidR.verbose") || getOption("lidR.debug"))
    cat(..., "\n")
}

dummy_las <- generate_las

uuid <- function()
{
  hex_digits <- c(as.character(0:9), letters[1:6])
  y_digits <- hex_digits[9:12]
  paste(
    paste0(sample(hex_digits, 8), collapse=''),
    paste0(sample(hex_digits, 4), collapse=''),
    paste0('4', sample(hex_digits, 3), collapse=''),
    paste0(sample(y_digits,1),
           sample(hex_digits, 3),
           collapse=''),
    paste0(sample(hex_digits, 12), collapse=''),
    sep='-')
}

fast_count_equal = function(x, v)
{
  if (is_compact(x))
  {
    if (x[1] == v)
      return(length(x))
    else
      return(0)
  }
  else
  {
    return(fast_countequal(x, v))
  }
}
