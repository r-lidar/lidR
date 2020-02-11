catalog_laxindex = function(ctg)
{
  stopifnot(is(ctg, "LAScatalog"))

  opt_chunk_size(ctg)   <- 0
  opt_chunk_buffer(ctg) <- 0
  opt_wall_to_wall(ctg) <- FALSE
  opt_output_files(ctg) <- ""

  create_lax_file = function(cluster)
  {
    rlas::writelax(cluster@files)
    return(0)
  }

  options <- list(need_buffer = FALSE, drop_null = FALSE)

  catalog_apply(ctg, create_lax_file,.options = options())
  return(invisible())
}
