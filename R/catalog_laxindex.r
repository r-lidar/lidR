catalog_laxindex = function(ctg)
{
  stopifnot(is(ctg, "LAScatalog"))
  ncores = cores(ctg)
  progress = progress(ctg)

  future::plan(future::multiprocess, workers = ncores)

  for(file in ctg@data$filename)
  {
    future::future({rlas::writelax(file) }, earlySignal = TRUE)
    cat(file, "\n")
  }
}