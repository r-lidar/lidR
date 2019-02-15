catalog_fakerun = function(ctg, sleep = 0.05)
{
  opt_wall_to_wall(ctg) <- FALSE
  options <- list(need_buffer = FALSE, drop_null = FALSE)

  fake = function(x)
  {
    Sys.sleep(sleep)
    if (stats::runif(1) < 0.05) warning("Fake warning", call. = FALSE)
    if (stats::runif(1) < 0.01) stop("Fake error", call. = FALSE)
    return(0)
  }

  catalog_apply(ctg, fake, .options = options)
  return(invisible())
}
