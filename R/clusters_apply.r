cluster_apply = function(clusters, f, processing_options, output_options, drop_null = TRUE, ...)
{
  stopifnot(is.list(clusters))
  assertive::assert_is_function(f)
  assertive::assert_is_a_bool(drop_null)

  nclust <- length(clusters)
  output <- vector("list", nclust)
  ncores <- if (nclust <= processing_options$core) nclust else processing_options$core
  codes  <- rep(ASYNC_RUN, nclust)
  dots   <- list(...)

  future::plan(future::multiprocess, workers = ncores)

  # Display the color legend over the LAScatalog that should have already been plotted by makecluster.
  if (processing_options$progress)
  {
    if (requireNamespace("progress", quietly = TRUE))
      pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = nclust, clear = FALSE)
    else
      pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

    graphics::legend("topright", title = "Colors", legend = c("No data","Ok","Errors (skipped)"), fill = c("gray","forestgreen", "red"), cex = 0.8)
  }

  # Parallel loop using asynchronous computation
  for (i in seq_along(clusters))
  {
    output[[i]] <- future::future(
    {
      x = f(clusters[[i]], ...)
      if (is.null(x)) return(NULL)
      if (clusters[[i]]@save == "") return(x)
      return(cluster_write(x, clusters[[i]]@save, output_options))
    }, substitute = TRUE)

    # Error handling and progress report
    for (j in 1:i)
    {
      if (codes[j] != ASYNC_RUN) next
      codes[j] = early_eval(output[[j]], processing_options$stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (processing_options$progress)
      {
        update_graphic(clusters[[j]]@bbox, codes[j])
        update_pb(pb, i/nclust)
      }
    }
  }

  # Because of asynchronous computation, the loop may be ended
  # but the computations not. Wait & check until the end.
  not_finished = which(codes == ASYNC_RUN)
  while(length(not_finished) > 0)
  {
    for (j in not_finished)
    {
      codes[j] = early_eval(output[[j]], processing_options$stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (processing_options$progress)
      {
        update_graphic(clusters[[j]]@bbox, codes[j])
        update_pb(pb, i/nclust)
      }
    }

    not_finished = which(codes == ASYNC_RUN)
    Sys.sleep(0.1)
  }

  if (any(codes == ASYNC_RUN)) stop("Unexpected error: a cluster is missing. Please contact the author.")
  if (drop_null) output <- output[codes != ASYNC_ERROR & codes != ASYNC_NULL]

  output <- future::values(output)
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

update_graphic = function(bbox, code)
{
  if (code == ASYNC_OK)
    col = "forestgreen"
  else if (code == ASYNC_NULL)
    col = "gray"
  else if (code == ASYNC_ERROR)
    col = "red"

  graphics::rect(bbox[1], bbox[2], bbox[3], bbox[4], border = "black", col = col)
}

update_pb = function(pb, ratio)
{
  pb_type = class(pb)[1]

  if (pb_type == "txtProgressBar")
    utils::setTxtProgressBar(pb, ratio)
  else
  {
    if(!pb$finished) pb$update(ratio)
  }
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
    driver <- output_options$drivers$SimpleFeature
    path   <- paste0(path, ".shp")
    driver$write(sf::st_as_sf(x), path)
    return(path)
  }
  else if (type == "SimpleFeature")
  {
    driver <- output_options$drivers$SimpleFeature
    path   <- paste0(path, ".shp")
    driver$write(x, path)
    return(path)
  }
  else if (type == "lidr_internal_skip_write")
  {
    # Nothing. This happens because sometime functions such as catalog_retile stream the data. So the called
    # function do the write job. If the called fwould unction return NULL the progress would be broken
    # (NULL means no data). Thus we return 0 with a class lidr_internal_skip_write
    return(0)
  }
  else if (type %in% c("data.frame", "data.table"))
  {
    driver <- output_options$drivers$SimpleFeature
    path   <- paste0(path, ".txt")
    driver$write(x, path)
    return(path)
  }
  else
    stop(glue::glue("Trying to write an object of class {type} but this type is not supported."))
}

# This was introduced in https://github.com/Jean-Romain/lidR/pull/159 and is expected to be no longer useful
# User supplied function not being analysed for globals/packages by the future we have to do it manually.
# Not sure it will be requiered in v2.0
# if (ncores > 1 & !future::supportsMulticore())
# {
#   is.fun <- vapply(dots, is.function, logical(1))
#
#   if(any(is.fun))
#   {
#     dots <- dots[is.fun]
#     for(fun in dots)
#     {
#       globals <- future::getGlobalsAndPackages(fun)
#       required.pkgs <- c(required.pkgs, setdiff(globals$packages, required.pkgs))
#
#       where   <- attr(globals$globals, "where")
#       pkgs    <- unlist(lapply(where, attr, "name"), use.names = FALSE)
#       pkgs    <- unique(grep("package\\:", pkgs, value = TRUE))
#       pkgs    <- gsub("package\\:", "", unique(pkgs))
#       required.pkgs <- c(required.pkgs, setdiff(pkgs, required.pkgs))
#     }
#   }
# }