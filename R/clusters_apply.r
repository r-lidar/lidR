cluster_apply = function(clusters, f, ncores, progress, stop_early, ...)
{
  stopifnot(is.list(clusters), is.function(f), is.integer(ncores), is.logical(progress), is.logical(stop_early))

  nclust <- length(clusters)
  output <- vector("list", nclust)
  ncores <- if (nclust <= ncores) nclust else ncores
  codes  <- rep(ASYNC_RUN, nclust)

  future::plan(future::multiprocess, workers = ncores)

  # User supplied function not being analysed for globals/packages by the future we have to do it manually.
  if (ncores > 1 & !future::supportsMulticore())
  {
    dots <- list(...)
    is.fun <- vapply(dots, is.function, logical(1))
    required.pkgs <- "lidR"

    if(any(is.fun))
    {
      dots <- dots[is.fun]
      for(fun in dots)
      {
        globals <- future::getGlobalsAndPackages(fun)
        required.pkgs <- c(required.pkgs, setdiff(globals$packages, required.pkgs))

        where   <- attr(globals$globals, "where")
        pkgs    <- unlist(lapply(where, attr, "name"), use.names = FALSE)
        pkgs    <- unique(grep("package\\:", pkgs, value = TRUE))
        pkgs    <- gsub("package\\:", "", unique(pkgs))
        required.pkgs <- c(required.pkgs, setdiff(pkgs, required.pkgs))
      }
    }
  }

  # Display the color legend over the LAScatalog that should have already been plotted.
  if (progress)
    graphics::legend("topright", title = "Colors", legend = c("No data","Ok","Errors (skipped)"), fill = c("gray","forestgreen", "red"), cex = 0.8)

  # Parallel loop using promises
  for (i in seq_along(clusters))
  {
    # Asynchronous computation
    output[[i]] <- future::future({ f(clusters[[i]], ...) }, substitute = TRUE, packages = required.pkgs)

    # Error handling and progress report
    for (j in 1:i)
    {
      if (codes[j] != ASYNC_RUN) next
      codes[j] = early_eval(output[[j]], stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (progress) display_progress(clusters[[j]]@bbox, i/nclust, codes[j])
    }
  }

  # Because of asynchronous computation, the loop may be ended
  # but the computations not. Wait & check until the end.
  not_finished = which(codes == ASYNC_RUN)
  while(length(not_finished) > 0)
  {
    for (j in not_finished)
    {
      codes[j] = early_eval(output[[j]], stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (progress) display_progress(clusters[[j]]@bbox, i/nclust, codes[j])
    }

    not_finished = which(codes == ASYNC_RUN)
    Sys.sleep(0.1)
  }

  if (progress) cat("\n")
  if (any(codes == ASYNC_RUN)) stop("Unexpected error: a cluster is missing. Please contact the author.")

  output = output[codes != ASYNC_ERROR & codes != ASYNC_NULL]
  output = future::values(output)
  return(output)
}

early_eval <- function(future, stop_early)
{
  code = ASYNC_RUN

  if (future::resolved(future))
  {
    code = tryCatch(
    {
      x = future::value(future)

      if (!is.null(x))
        return(ASYNC_OK)
      else
        return(ASYNC_NULL)
    }, error = function(e) {
      if (stop_early)
        stop(e)
      else
        return(ASYNC_ERROR)
    })
  }

  return(code)
}

display_progress = function(bbox, p, code)
{
  cat(sprintf("\rProgress: %g%%", round(p*100)), file = stderr())

  if (code == ASYNC_OK)
    col = "forestgreen"
  else if (code == ASYNC_NULL)
    col = "gray"
  else if (code == ASYNC_ERROR)
    col = "red"

  graphics::rect(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, border = "black", col = col)
}