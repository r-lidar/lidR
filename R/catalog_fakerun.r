catalog_fakerun = function(ctg, sleep = 0.05)
{
  opt_wall_to_wall(ctg) <- FALSE
  options <- list(need_buffer = FALSE, drop_null = FALSE)
  catalog_apply(ctg, function(x){ Sys.sleep(sleep) ; return(0) }, .options = options)
  return(invisible())
}
