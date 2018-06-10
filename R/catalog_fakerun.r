catalog_fakerun = function(ctg, sleep = 0.05)
{
  clusters = catalog_makecluster(ctg, 1)
  cluster_apply(clusters, function(x){Sys.sleep(sleep) ; return (0)}, 1L, progress(ctg), FALSE)
  return(invisible())
}