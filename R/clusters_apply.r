cluster_apply = function(clusters, f, ncores, progress, ...)
{
  stopifnot(is.function(f), is.logical(progress))
  nclust <- length(clusters)
  output <- list()
  ncores <- if (nclust <= ncores) nclust else ncores

  future::plan(future::multiprocess, workers = ncores)

  for (i in seq_along(clusters))
  {
    cluster <- clusters[[i]]
    output[[i]] <- future::future({ f(cluster, ...) }, earlySignal = TRUE)

    if (progress)
    {
      cat(sprintf("\rProgress: %g%%", round(i/nclust*100)), file = stderr())
      graphics::rect(cluster@bbox$xmin, cluster@bbox$ymin, cluster@bbox$xmax, cluster@bbox$ymax, border = "black", col = "forestgreen")
    }
  }

  if (progress) cat("\n")

  output = future::values(output)

  return(output)
}