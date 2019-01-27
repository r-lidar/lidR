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

cluster_apply = function(clusters, FUN, processing_options, output_options, drop_null = TRUE, ...)
{
  stopifnot(is.list(clusters))
  assert_is_function(FUN)
  assert_is_a_bool(drop_null)

  nclust <- length(clusters)
  output <- vector("list", nclust)
  params <- list(...)
  prgrss <- processing_options$progress

  # Progress estimation
  if (prgrss)
  {
    if (requireNamespace("progress", quietly = TRUE))
      pb <- progress::progress_bar$new(format = glue::glue("Processing [:bar] :percent (:current/:total) eta: :eta"), total = nclust, clear = FALSE)
    else
      pb <- utils::txtProgressBar(min = 0, max = 1, style = 3)

    graphics::legend("topright", title = "Colors", legend = c("Empty","Ok","Warning", "Errors"), fill = c("gray","forestgreen", "orange", "red"), cex = 0.8)
  }

  # Find the name of the first paramter of FUN
  # (it can be anything because FUN might be a user-defined function)
  formal_f <- formals(FUN)
  first_p  <- names(formal_f)[1]

  for (i in seq_along(clusters))
  {
    cluster <- clusters[[i]]
    params[[first_p]] <- cluster
    cluster_state <- CHUNK_OK
    cluster_msg   <- ""

    y <- tryCatch(
    {
      do.call(FUN, params)
    },
    error = function(e)
    {
      cluster_state <<- CHUNK_ERROR
      cluster_msg   <<- e
    },
    warning = function(w)
    {
      cluster_state <<- CHUNK_WARNING
      cluster_msg   <<- w
    })

    if (is.null(y))
    {
      cluster_state <- CHUNK_NULL
    }

    if (prgrss)
    {
      update_graphic(cluster, cluster_state)
      update_pb(pb, i/nclust)
    }

    if (cluster_state == CHUNK_ERROR & processing_options$stop_early)
      stop(cluster_msg)

    if (cluster_state == CHUNK_WARNING)
      warning(cluster_msg)

    if (cluster_state == CHUNK_NULL | cluster_state == CHUNK_ERROR)
      output[i]   <- list(NULL)
    else if (cluster@save == "")
      output[[i]] <- y
    else
      output[[i]] <- writeANY(y, cluster@save, output_options$drivers)
  }

  if (drop_null)
    output <- Filter(Negate(is.null), output)

  return(output)
}

update_graphic = function(cluster, code)
{
  if (is.null(cluster))
    return(NULL)

  bbox = cluster@bbox

  if (code == CHUNK_OK)
    col = "forestgreen"
  else if (code == CHUNK_NULL)
    col = "gray"
  else if (code == CHUNK_WARNING)
    col = "orange"
  else if (code == CHUNK_ERROR)
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
