catalog_laxindex = function(ctg)
{
  stopifnot(is(ctg, "LAScatalog"))

  by_file(ctg) <- TRUE
  buffer(ctg)  <- 0

  clusters  <- catalog_makecluster(ctg, 1)

  ncores    <- cores(ctg)
  progress  <- progress(ctg)
  stopearly <- stop_early(ctg)

  cluster_apply(clusters, create_lax_file, ncores, progress, stopearly)

  return(invisible())
}

create_lax_file = function(cluster)
{
  rlas::writelax(cluster@files)
  return(0)
}