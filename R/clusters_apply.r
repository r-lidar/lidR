cluster_apply = function(clusters, f, processing_options, output_options, drop_null = TRUE, ...)
{
  stopifnot(is.list(clusters))
  assertive::assert_is_function(f)
  assertive::assert_is_a_bool(drop_null)

  nclust <- length(clusters)
  output <- vector("list", nclust)
  ncores <- if (nclust <= processing_options$core) nclust else processing_options$core
  codes  <- rep(ASYNC_RUN, nclust)

  future::plan(future::multiprocess, workers = ncores)

  required.pkgs <- "lidR"

  # User supplied function not being analysed for globals/packages by the future we have to do it manually.
  # Not sure it will be requiered in v2.0
  if (ncores > 1 & !future::supportsMulticore())
  {
    dots <- list(...)
    is.fun <- vapply(dots, is.function, logical(1))

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
  if (processing_options$progress)
    graphics::legend("topright", title = "Colors", legend = c("No data","Ok","Errors (skipped)"), fill = c("gray","forestgreen", "red"), cex = 0.8)

  # Parallel loop using asynchronous computation
  for (i in seq_along(clusters))
  {
    output[[i]] <- future::future(
    {
      x = f(clusters[[i]], ...)
      if (is.null(x)) return(NULL)
      if (clusters[[i]]@save == "") return(x)
      return(cluster_write(x, clusters[[i]]@save, output_options))
    }, substitute = TRUE, packages = required.pkgs)

    # Error handling and progress report
    for (j in 1:i)
    {
      if (codes[j] != ASYNC_RUN) next
      codes[j] = early_eval(output[[j]], processing_options$stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (processing_options$progress) display_progress(clusters[[j]]@bbox, i/nclust, codes[j])
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
      if (processing_options$progress) display_progress(clusters[[j]]@bbox, i/nclust, codes[j])
    }

    not_finished = which(codes == ASYNC_RUN)
    Sys.sleep(0.1)
  }

  if (processing_options$progress) cat("\n")
  if (any(codes == ASYNC_RUN)) stop("Unexpected error: a cluster is missing. Please contact the author.")

  if (drop_null)
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

cluster_write = function(x, path, output_options)
{
  type = class(x)

  if (type == "LAS")
  {
    driver <- output_options$drivers$LAS
    ext    <- if (driver$laz_compression) ".laz" else ".las"
    path   <- paste0(path, ext)
    driver$write(x, path)
    return(path)
  }
  else if (type %in% c("RasterLayer", "RasterBrick", "RasterStack"))
  {
    driver <- output_options$drivers$Raster
    path   <- paste0(path, ".tif")
    driver$write(x, path, driver$format)
    return(path)
  }
  else if (type %in% c("SpatialPoints", "SpatialPointsDataFrame", "SpatialPolygons", "SpatialPolygonsDataFrame", "SpatialLines", "SpatialLinesDataFrame"))
  {

  }
  else if (type == "SimpleFeature")
  {

  }
  else
    stop(glue::glue("Trying to write an object of class {type} but this type is not supported."))
}