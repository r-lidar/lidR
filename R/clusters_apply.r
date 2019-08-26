# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2019 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================

cluster_apply = function(.CLUSTER, .FUN, .PROCESSOPT, .OUTPUTOPT, .GLOBALS = NULL, ...)
{
  # Parse ellipsis
  params  <- list(...)
  first_p <- names(formals(.FUN))[1]

  # Initialize output
  nclusters  <- length(.CLUSTER)
  futures    <- vector("list", nclusters)
  output     <- vector("list", nclusters)
  writemode  <- .CLUSTER[[1]]@save != ""
  drivers    <- .OUTPUTOPT$drivers

  # Initialize progress bar
  prgrss     <- .PROCESSOPT$progress
  abort      <- .PROCESSOPT$stop_early
  states     <- rep(CHUNK_WAINTING, nclusters)
  messages   <- rep("", nclusters)
  pb         <- engine_progress_bar(nclusters, prgrss)
  percentage <- 0

  # Inititalize parallelism
  workers    <- getWorkers()
  threads    <- getThreads()
  cores      <- future::availableCores()
  manual     <- getOption("lidR.threads.manual")

  if (!manual && workers * threads > cores)
  {
    verbose(glue::glue("Cannot nest {workers} future threads and {threads} OpenMP threads. Precedence given to future: OpenMP threads set to 1."))
    threads <- 1L
  }

  verbose(glue::glue("Start processing {nclusters} chunks..."))
  verbose(glue::glue("Using {workers} CPUs with future and {threads} CPU with OpenMP."))

  # ==== PROCESSING ====

  for (i in seq_along(.CLUSTER))
  {
    params[[first_p]] <- .CLUSTER[[i]]
    save <- .CLUSTER[[i]]@save

    states[i] <- CHUNK_PROCESSING
    engine_update_progress(pb, .CLUSTER[[i]], states[i], percentage, i)

    # Asynchronous computation of .FUN on the chunk
    futures[[i]] <- future::future(
    {
      setThreads(threads)
      options(lidR.progress = FALSE)
      options(lidR.verbose = FALSE)
      y <- do.call(.FUN, params)
      if (is.null(y)) y <- NULL
      if (!is.null(y) && writemode) y <- writeANY(y, save, drivers)
      y
    }, substitute = TRUE, globals = structure(TRUE, add = .GLOBALS))

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
      if (states[j] == CHUNK_PROCESSING) next

      # The state changed: the chunk was processed. Update the progress
      percentage <-  engine_compute_progress(states)
      engine_update_progress(pb, .CLUSTER[[j]], states[j], percentage, j)

      # The state is ERROR: abort the process nicely
      if (states[j] == CHUNK_ERROR & abort)
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
          engine_save_logs(.CLUSTER[[j]], j)
          message(messages[j])
          return(output)
        }
      }

      # The state is NULL: do nothing
      if (states[j] == CHUNK_NULL) next

      # The state is OK or WARNING: get the value
      output[[j]] <- suppressWarnings(future::value(futures[[j]]))
    }
  }

  # ==== PROGRESS ENDING ====

  # Because of asynchronous computation, the loop may be ended
  # but not the computations. Wait until the end & check states.

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
      engine_update_progress(pb, .CLUSTER[[j]], states[j], percentage, j)

      if (states[j] == CHUNK_ERROR & abort)
      {
        if (j == 1)
        {
          stop(messages[j], call. = FALSE)
        }
        else
        {
          engine_save_logs(.CLUSTER[[j]], j)
          message(messages[j])
          return(output)
        }
      }

      if (states[j] == CHUNK_NULL) next

      output[[j]] <- suppressWarnings(future::value(futures[[j]]))
    }

    Sys.sleep(0.5)
  }

  return(output)
}
engine_eval_state <- function(future)
{
  cluster_state <- list(state = CHUNK_PROCESSING, msg = "")

  if (is.null(future))
  {
    stop("Unexpected internal error: NULL received instead of a future. Please report this bug.", call. = FALSE)
  }

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

  if (!interactive())
    return(n)

  if (requireNamespace("progress", quietly = TRUE))
    pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = n, clear = FALSE)
  else
    pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

  graphics::legend("topright", title = "Colors", legend = c("Processing", "Empty","Ok","Warning", "Error"), fill = c("cornflowerblue", "gray","green3", "orange", "red"), cex = 0.8)

  return(pb)
}

engine_update_progress <- function(pb, cluster, state, p, j)
{
  if (is.null(pb))
    return(invisible(NULL))

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

  bbox <- cluster@bbox

  graphics::rect(bbox[1], bbox[2], bbox[3], bbox[4], border = "black", col = col)

  if (is(pb, "txtProgressBar"))
    utils::setTxtProgressBar(pb, p)
  else
    pb$update(p)

  return(invisible())
}

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
