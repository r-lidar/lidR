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

cluster_apply = function(clusters, FUN, processing_options, output_options, globals = NULL, ...)
{
  # Parse ellipsis
  params  <- list(...)
  first_p <- names(formals(FUN))[1]

  # Initialize output
  nclusters  <- length(clusters)
  futures    <- vector("list", nclusters)
  output     <- vector("list", nclusters)
  writemode  <- clusters[[1]]@save != ""
  drivers    <- output_options$drivers

  # Initialize progress bar
  prgrss     <- processing_options$progress
  abort      <- processing_options$stop_early
  states     <- rep(CHUNK_WAINTING, nclusters)
  pb         <- engine_progress_bar(nclusters, prgrss)

  # Intitalize parallelism
  workers    <- getWorkers()
  threads    <- getThreads()
  cores      <- future::availableCores()
  manual     <- getOption("lidR.threads.manual")

  if (!manual && workers * threads > cores)
  {
    verbose(glue::glue("Cannot nest {workers} future threads and {threads} OpenMP threads. Precedence given to future: OpenMP threads set to 1."))
    threads <- 1L
  }

  # ==== PROCESSING ====

  for (i in seq_along(clusters))
  {
    params[[first_p]] <- clusters[[i]]
    save <- clusters[[i]]@save

    # Asynchronous computation of FUN on the chunk
    futures[[i]] <- future::future(
    {
      setThreads(threads)
      y <- do.call(FUN, params)
      if (is.null(y)) return(NULL)
      if (!writemode) return(y)
      return(writeANY(y, save, drivers))
    }, substitute = TRUE, globals = structure(TRUE, add = globals))

    # Evaluation of the state of the futures
    for (j in 1:i)
    {
      # Skip chunks that were already evaluated and for which the state is known
      if (states[j] != CHUNK_WAINTING) next

      # Evaluate the state of the chunk
      states[j] <- engine_eval_state(futures[[j]])

      # The state is unchanged: the chunk is still processing
      if (states[j] == CHUNK_WAINTING) next

      # The state changed: the chunk was processed. Update the progress
      engine_update_progress(pb, clusters[[j]], states[j], sum(states != CHUNK_WAINTING)/nclusters)

      # The state is ERROR: abort the process nicely
      if (states[j] == CHUNK_ERROR & abort)
      {
        # If it fails in first chunk it is likely to be an error in code.
        # Stop and display the error message
        if (j == 1)
        {
          future::value(futures[[j]])
        }
        # If it fails somewhere else it is likely to be an error in a specific point cloud.
        # Return a partial output and display the logs
        else
        {
          engine_save_logs(clusters[[j]], j)
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

  while (any(states == CHUNK_WAINTING))
  {
    i <- which(states == CHUNK_WAINTING)

    for (j in i)
    {
      if (states[j] != CHUNK_WAINTING) next

      states[j] <- engine_eval_state(output[[j]])

      if (states[j] == CHUNK_WAINTING) next

      engine_update_progress(pb, clusters[[j]], states[j], sum(states != CHUNK_WAINTING)/nclusters)

      if (states[j] == CHUNK_ERROR & abort)
      {
        if (j == 1)
        {
          future::value(futures[[j]])
        }
        else
        {
          engine_save_logs(clusters[[j]], j)
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
  cluster_state <- CHUNK_WAINTING

  if (future::resolved(future))
  {
    cluster_state <- CHUNK_OK

    tryCatch(
      {
        withCallingHandlers(
          {
            y <- future::value(future)
            if (is.null(y)) cluster_state <- CHUNK_NULL
          },
          warning = function(w)
          {
            cluster_state <<- CHUNK_WARNING
          })
      },
      error = function(e)
      {
        cluster_state <<- CHUNK_ERROR
      })
  }

  return(cluster_state)
}

engine_progress_bar <- function(n, prgss = FALSE)
{
  pb <- NULL

  if (!prgss)
    return(pb)

  if (requireNamespace("progress", quietly = TRUE))
    pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = n, clear = FALSE)
  else
    pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

  graphics::legend("topright", title = "Colors", legend = c("Empty","Ok","Warning", "Error"), fill = c("gray","forestgreen", "orange", "red"), cex = 0.8)

  return(pb)
}

engine_update_progress <- function(pb, cluster, state, p)
{
  if (!is.null(pb))
  {
    bbox <- cluster@bbox

    if (state == CHUNK_OK)
      col <- "forestgreen"
    else if (state == CHUNK_NULL)
      col <- "gray"
    else if (state == CHUNK_WARNING)
      col <- "orange"
    else if (state == CHUNK_ERROR)
      col <- "red"

    graphics::rect(bbox[1], bbox[2], bbox[3], bbox[4], border = "black", col = col)

    if (is(pb, "txtProgressBar"))
      utils::setTxtProgressBar(pb, p)
    else
      pb$update(p)
  }
}

engine_save_logs <- function(cluster, index)
{
  log <- glue::glue("{tempdir()}/chunk{index}.rds")
  saveRDS(cluster, log)
  message(glue::glue("\nAn error occurred when processing the chunk {index}. Try to load this chunk with:\n chunk <- readRDS(\"{log}\")\n las <- readLAS(chunk)"))
}
