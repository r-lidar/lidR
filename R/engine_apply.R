#' @rdname engine
#' @export
#' @param .CHUNKS list. list of LAScluster
#' @param .FUN function. function that respects a template (see \link{catalog_apply})
#' @param .PROCESSOPT list. Processing option
#' @param .OUTPUTOPT list. Output option
#' @param .GLOBALS list. Force export of some object in workers
#' @param .AUTOREAD bool. Enable autoread
#' @param .AUTOCROP bool. Enable autocrop
#' @param ... parameters of .FUN
engine_apply = function(.CHUNKS, .FUN, .PROCESSOPT, .OUTPUTOPT, .GLOBALS = NULL, .AUTOREAD = FALSE, .AUTOCROP = FALSE, ...)
{
  # Parse ellipsis
  params  <- list(...)

  # Retrieve the names of the param in user-defined function
  first_p <- names(formals(.FUN))[1]
  second_p <- names(formals(.FUN))[2]

  # Initialize output
  nclusters  <- length(.CHUNKS)
  futures    <- vector("list", nclusters)
  output     <- vector("list", nclusters)
  writemode  <- .CHUNKS[[1]]@save != ""
  drivers    <- .OUTPUTOPT$drivers

  # Initialize progress bar
  prgrss     <- .PROCESSOPT$progress
  abort      <- .PROCESSOPT$stop_early
  states     <- rep(CHUNK_WAINTING, nclusters)
  messages   <- rep("", nclusters)
  pb         <- engine_progress_bar(nclusters, prgrss)
  percentage <- 0

  raster.default <- getOption("lidR.raster.default")
  on.exit(engin_close_pb(pb))

  # Disable OpenMP? The different between  LIDRTHREADS$n and LIDRTHREADS$input
  # is that n is the corrected number of workers. e.g on a quadcore set_lidr_threads(0)
  # set 4 and set_lidr_threads(12) set 4 too. On the contrary input will be 0 and 12 respectively
  # Consequently so on remote machines with different capabilities than the master worker
  # the information is not overwritten by the master worker.
  threads = if (must_disable_openmp()) 1L else if (isFALSE(getOption("lidR.check.nested.parallelism"))) LIDRTHREADS$input else LIDRTHREADS$n

  # ==== PROCESSING ====
  use_future <- engine_use_future()
  if (!use_future) verbose("Future is disabled")
  verbose(glue::glue("Start processing {nclusters} chunks..."))

  future <- if (!use_future)  no_future else future::future
  value  <- if (!use_future) function(x) { attr(x, "state") <- NULL ; x } else function(x) { suppressWarnings(future::value(x)) }

  for (i in seq_along(.CHUNKS))
  {
    chunk <- .CHUNKS[[i]]
    params[[first_p]] <- chunk
    save <- chunk@save

    states[i] <- CHUNK_PROCESSING
    engine_update_progress(pb, .CHUNKS[[i]], states[i], percentage, i)

    # Asynchronous computation of .FUN on the chunk
    futures[[i]] <- future(
    {
      setThreads(threads)
      options(lidR.progress = FALSE)
      options(lidR.verbose = FALSE)
      options(lidR.raster.default = raster.default)
      y <- NULL

      # Regular behaviour before v3.0.0
      if (.AUTOREAD == FALSE)
      {
        y <- do.call(.FUN, params)
      }
      # New option autoread from v3.0.0 (was not a good idea but it exists)
      else if (.AUTOREAD == TRUE & .AUTOCROP == FALSE)
      {
        bbox <- st_bbox(chunk)
        las  <- readLAS(chunk)
        y    <- NULL
        if (!is.empty(las))
        {
          params[[first_p]] <- las
          params[[second_p]] <- bbox
          y <- do.call(.FUN, params)
        }
      }
      # New option autocrop = TRUE from v4.0.0 intends to make simplify the logic of catalog apply.
      # it is not documented and is only used in apply.LAScatalog
      else if (.AUTOREAD == TRUE & .AUTOCROP == TRUE)
      {
        bbox <- st_bbox(chunk)
        las  <- readLAS(chunk)
        y    <- NULL
        if (!is.empty(las))
        {
          params[[first_p]] <- las
          y <- do.call(.FUN, params)
        }

        if (!is.null(y))
          y <- engine_crop(y, bbox)
      }
      else
        stop("Internal error in engine_apply: AUTO options are invalid. Please report.", call. = FALSE)

      if (is.null(y)) y <- NULL
      if (!is.null(y) && writemode) y <- engine_write(y, save, drivers)

      # Fix #523
      if (is(y, "SpatRaster") | is(y, "SpatVector")) y <- terra::wrap(y)

      y
    }, substitute = TRUE, globals = structure(TRUE, add = .GLOBALS), seed = TRUE)

    # Evaluation of the state of the futures
    for (j in 1:i)
    {
      # Skip chunks that were already evaluated and for which the state is known
      if (states[j] != CHUNK_PROCESSING) next

      # Evaluate the state of the chunk
      state       <- engine_eval_state(futures[[j]])
      states[j]   <- state[["state"]]
      messages[j] <- state[["msg"]]

      # The state is unchanged: the chunk is still processing
      if (states[j] == CHUNK_PROCESSING) next # nocov

      # The state changed: the chunk was processed. Update the progress
      percentage <-  engine_compute_progress(states)
      engine_update_progress(pb, .CHUNKS[[j]], states[j], percentage, j)

      # The state is ERROR: abort the process nicely
      if (states[j] == CHUNK_ERROR && abort)
      {
        # If it fails in first chunk it is likely to be an error in code.
        # Stop and display the error message
        if (j == 1)
        {
          stop(messages[j], call. = FALSE)
        }
        # If it fails somewhere else it is likely to be an error in a specific point cloud.
        # Return a partial output and display the logs
        else
        {
          engine_save_logs(.CHUNKS[[j]], j)
          message(messages[j])
          return(output)
        }
      }

      if (states[j] == CHUNK_ERROR && !abort) {
        output[[j]] <- NULL
        next
      }

      # The state is NULL: do nothing
      if (states[j] == CHUNK_NULL) next

      # The state is OK or WARNING: get the value
      res <- value(futures[[j]])

      # Fix #523
      if (is(res, "PackedSpatRaster"))
        res <- terra::rast(res)
      if (is(res, "PackedSpatVector"))
        res <- terra::vect(res)

      output[[j]] <- res
    }
  }

  # ==== PROGRESS ENDING ====

  # Because of asynchronous computation, the loop may be ended
  # but not the computations. Wait until the end & check states.
  # no cov because tested with a single core on CRAN

  # nocov start
  while (any(states == CHUNK_PROCESSING))
  {
    i <- which(states == CHUNK_PROCESSING)

    for (j in i)
    {
      if (states[j] != CHUNK_PROCESSING) next

      state       <- engine_eval_state(futures[[j]])
      states[j]   <- state[["state"]]
      messages[j] <- state[["msg"]]

      if (states[j] == CHUNK_PROCESSING) next

      percentage <-  engine_compute_progress(states)
      engine_update_progress(pb, .CHUNKS[[j]], states[j], percentage, j)

      if (states[j] == CHUNK_ERROR && abort)
      {
        if (j == 1)
        {
          stop(messages[j], call. = FALSE)
        }
        else
        {
          engine_save_logs(.CHUNKS[[j]], j)
          message(messages[j])
          return(output)
        }
      }

      if (states[j] == CHUNK_ERROR && !abort) {
        output[[j]] <- NULL
        next
      }

      if (states[j] == CHUNK_NULL) next

      res <- value(futures[[j]])

      # Fix #523
      if (is(res, "PackedSpatRaster"))
        res <- terra::rast(res)
      if (is(res, "PackedSpatVector"))
        res <- terra::vect(res)

      output[[j]] <- res
      attr(output[[j]], "state") <- NULL ;
    }

    Sys.sleep(0.5)
  }

  return(output)
}
# nocov end

engine_eval_state <- function(future)
{

  # If future is not a future it means that we are in "no future" mode
  # The result has already been evaluated and the state is stored in an attribute
  if (!is(future, "Future")) return(attr(future, "state"))

  # future is a future, we must check if it is resolved. If it is resolved we evaluate
  # the state of the result

  # Fix #414
  sink(paste0(tempdir(), "/dev_null"))
  on.exit(sink(NULL))

  cluster_state <- list(state = CHUNK_PROCESSING, msg = "")

  if (future::resolved(future))
  {
    cluster_state <- list(state = CHUNK_OK, msg = "")

    tryCatch(
    {
      withCallingHandlers(
      {
        y <- future::value(future)
        if (is.null(y)) cluster_state <- list(state = CHUNK_NULL, msg = "")
      },
      warning = function(w)
      {
        cluster_state <<- list(state = CHUNK_WARNING, msg = w["message"])
      })
    },
    error = function(e)
    {
      cluster_state <<- list(state = CHUNK_ERROR, msg = e["message"])
    })
  }

  return(cluster_state)
}

engine_progress_bar <- function(n, prgss = FALSE)
{
  pb <- NULL

  if (!prgss)
    return(pb)

  # nocov start
  if (!interactive())
    return(n)

  if (requireNamespace("progress", quietly = TRUE))
    pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = n, clear = FALSE)
  else
    pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

  graphics::legend("topright", title = "Colors", legend = c("Processing", "Empty","Ok","Warning", "Error"), fill = c("cornflowerblue", "gray","green3", "orange", "red"), cex = 0.8)

  return(pb) # nocov end
}

engine_update_progress <- function(pb, cluster, state, p, j)
{
  if (is.null(pb))
    return(invisible(NULL))

  # nocov start
  if (state == CHUNK_OK) { col <- "green3" ; sym <- "\u2713" }
  else if (state == CHUNK_NULL) { col <- "gray" ; sym <- "\u2205" }
  else if (state == CHUNK_WARNING) { col <- "orange" ; sym <- "\u26A0" }
  else if (state == CHUNK_ERROR) { col <- "red" ; sym <- "\u2717" }
  else if (state == CHUNK_PROCESSING) { col <- "cornflowerblue" ; sym <- "\u21BB" }

  if (!interactive())
  {
    if (state == CHUNK_PROCESSING)
      return(invisible())

    cat(glue::glue("Chunk {j} of {pb} ({round(p*100,1)}%): state {sym}"))
    cat("\n")
    return(invisible())
  }

  if (cluster@shape == LIDRRECTANGLE) {
    if (cluster@wkt == "") {
      bbox <- cluster@bbox
      graphics::rect(bbox[1], bbox[2], bbox[3], bbox[4], border = "black", col = col)
    } else {
      poly <- sf::st_as_sfc(cluster@wkt)
      plot(poly, add = TRUE, col = col)
    }
  } else if (cluster@shape == LIDRCIRCLE) {
    center <- cluster@center
    width  <- cluster@width
    theta <- seq(0, 2 * pi, length = 32)
    graphics::polygon(x = center$x + width/2 * cos(theta), y = center$y + width/2 * sin(theta), col = col)
  }

  if (is(pb, "txtProgressBar"))
    utils::setTxtProgressBar(pb, p)
  else
    pb$update(p)

  return(invisible(NULL)) # nocov end
}

# nocov start
engin_close_pb <- function(pb)
{
  if (is.null(pb))
    return(invisible(NULL))

  if (!interactive())
    return(invisible(NULL))

  if (is(pb, "txtProgressBar"))
    close(pb)
  else if (!pb$finished)
    pb$terminate()

  return(invisible(NULL))
}
# nocov end

engine_save_logs <- function(cluster, index)
{
  log <- glue::glue("{tempdir()}/chunk{index}.rds")
  saveRDS(cluster, log)
  message(glue::glue("\nAn error occurred when processing the chunk {index}. Try to load this chunk with:\n chunk <- readRDS(\"{log}\")\n las <- readLAS(chunk)"))
}

engine_compute_progress = function(states)
{
  sum(states != CHUNK_WAINTING & states != CHUNK_PROCESSING)/length(states)
}

engine_use_future <- function()
{
  if (isTRUE(getOption("lidR.debug")))
    return(FALSE)

  if (isTRUE(getOption("lidR.no.future")))
    return(FALSE)

  b <- requireNamespace("future", quietly = TRUE)
  return(b)
}

no_future <- function(expr, ...)
{
  cluster_state <- list(state = CHUNK_OK, msg = "")

  y <- tryCatch(
  {
    withCallingHandlers(
    {
      y <- eval(expr)
      if (is.null(y)) cluster_state <- list(state = CHUNK_NULL, msg = "")
      y
    },
    warning = function(w)
    {
      cluster_state <<- list(state = CHUNK_WARNING, msg = w["message"])
    })
  },
  error = function(e)
  {
    cluster_state <<- list(state = CHUNK_ERROR, msg = e["message"])
  })

  if (is.null(y)) y <- list(NULL)
  attr(y, "state") <- cluster_state
  return(y)
}
