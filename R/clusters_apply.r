# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

cluster_apply = function(clusters, FUN, processing_options, output_options, drop_null = TRUE, globals = NULL, ...)
{
  stopifnot(is.list(clusters))
  assert_is_function(FUN)
  assert_is_a_bool(drop_null)

  nclust <- length(clusters)
  output <- vector("list", nclust)
  ncores <- if (nclust <= processing_options$core) nclust else processing_options$core
  plan   <- processing_options$plan
  codes  <- rep(ASYNC_RUN, nclust)
  params <- list(...)

  future::plan(plan, workers = ncores)

  # Progress estimation
  if (processing_options$progress)
  {
    if (requireNamespace("progress", quietly = TRUE))
      pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = nclust, clear = FALSE)
    else
      pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

    graphics::legend("topright", title = "Colors", legend = c("No data","Ok","Errors (skipped)"), fill = c("gray","forestgreen", "red"), cex = 0.8)
  }

  # Find the name of the first paramter of FUN (it can be anything because FUN might be a user-defined function)
  formal_f <- formals(FUN)
  first_p  <- names(formal_f)[1]

  # Parallel loop using asynchronous computation
  for (i in seq_along(clusters))
  {
    # Add the current LAScluster into params of function FUN
    current_processed_cluster <- clusters[[i]]

    # !!! A clusters might be NULL?? I don't remember in which case !!!
    if (!is.null(current_processed_cluster))
      params[[first_p]] <- current_processed_cluster
    else
      params[first_p] <- list(NULL)

    # Asynchronous computation of FUN
    output[[i]] <- future::future(
    {
      x <- do.call(FUN, params)
      if (is.null(x)) return(NULL)
      if (current_processed_cluster@save == "") return(x)                          # Return the output in R
      return(writeANY(x, current_processed_cluster@save, output_options$drivers))  # Write the output in file
    }, substitute = TRUE, globals = structure(TRUE, add = globals))

    # Error handling and progress report
    for (j in 1:i)
    {
      if (codes[j] != ASYNC_RUN) next
      codes[j] = early_eval(output[[j]], processing_options$stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (processing_options$progress)
      {
        update_graphic(clusters[[j]], codes[j])
        update_pb(pb, sum(codes != ASYNC_RUN)/length(codes))
      }
    }
  }

  # Because of asynchronous computation, the loop may be ended
  # but not the computations. Wait until the end & check.
  not_finished = which(codes == ASYNC_RUN)
  while (length(not_finished) > 0)
  {
    for (j in not_finished)
    {
      codes[j] = early_eval(output[[j]], processing_options$stop_early)
      if (codes[j] == ASYNC_RUN) next
      if (processing_options$progress)
      {
        update_graphic(clusters[[j]], codes[j])
        update_pb(pb, sum(codes != ASYNC_RUN)/length(codes))
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

update_graphic = function(cluster, code)
{
  if (is.null(cluster))
    return(NULL)

  bbox = cluster@bbox

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
    if (!pb$finished) pb$update(ratio)
  }
}
