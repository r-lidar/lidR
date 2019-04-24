catalog_fakerun = function(ctg, sleep = 0.05, pwarning = 0.05, perror = 0.01)
{
  opt_wall_to_wall(ctg) <- FALSE
  options <- list(need_buffer = FALSE, drop_null = FALSE)

  fake = function(x)
  {
    Sys.sleep(sleep)
    if (stats::runif(1) < pwarning) warning("Fake warning", call. = FALSE)
    if (stats::runif(1) < perror) stop("Fake error", call. = FALSE)
    return(0)
  }

  return(catalog_apply(ctg, fake, .options = options))
}
