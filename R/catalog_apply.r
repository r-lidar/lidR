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



#' LAScatalog processing engine
#'
#' This function gives access at the user level to the \link[lidR:LAScatalog-class]{LAScatalog} processing
#' engine. It allows the application of a user-defined routine over an entire catalog. The LAScatalog
#' processing engine tool is explained in the \link[lidR:LAScatalog-class]{LAScatalog class}\cr\cr
#' \strong{Warning:} the LAScatalog processing engine has a mechanism to load buffered data to avoid
#' edge artifacts, but no mechanism to remove the buffer after applying user-defined functions, since
#' this task is specific to each process. In other \code{lidR} functions this task is performed
#' specifically for each function. In \code{catalog_apply} the user's function can return any input,
#' thus users must take care of this task themselves (See section "Edge artifacts")
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object.
#' @param FUN A user-defined function that respects a given template (see section function template)
#' @param ... Optional arguments to FUN.
#'
#' @section Edge artifacts:
#'
#' It is important to take precautions to avoid 'edge artifacts' when processing wall-to-wall
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process or dummy elevations in a ground interpolation. The LAScatalog
#' processing engine provides internal tools to load buffered data. However, there is
#' no mechanism to remove the results computed in the buffered area since this task depends on the
#' output of the user-defined function. The user must take care of this task (see also examples).
#'
#' @section Buffered data:
#'
#' The LAS objects read by the user function have a special attribute called 'buffer' that indicates,
#' for each point, if it comes from a buffered area or not. Points from non-buffered areas have a
#' 'buffer' value of 0, while points from buffered areas have a 'buffer' value of 1, 2, 3 or 4, where
#' 1 is the bottom buffer and 2, 3 and 4 are the left, top and right buffers, respectively.
#'
#' @section Function template:
#'
#' The parameter \code{FUN} expects a function that has a first argument that will be supplied automatically
#' by the \code{LAScatalog} processing engine. This first argument is a \code{LAScluster}. A \code{LAScluster}
#' is an internal undocumented class but the user needs to know only three things about this class:
#' \itemize{
#' \item It represents a chunk of the catalog
#' \item The function \link{readLAS} can be used with a \code{LAScluster}
#' \item The function \link[raster:extent]{extent} or \link[sp:bbox]{bbox} can be used with a \code{LAScluster}
#' and it returns the bouding box of the cluster without the buffer. It can be used to clip the ouput
#' and remove the buffered region (see examples).
#' }
#' A user-defined function must be templated like this:
#' \preformatted{
#' myfun = function(cluster, ...)
#' {
#'    las = readLAS(cluster)
#'    if (is.empty(las)) return(NULL)
#'    # do something
#'    return(something)
#' }}
#' The line \code{if(is.empty(las)) return(NULL)} is important because some clusters (chunk) may contain
#' 0 points (we can't know that before reading the file). In this case an empty point cloud with 0 points
#' is returned by \code{readLAS} and this may fail in subsequent code. Thus, exiting early from the user-function
#' by returning \code{NULL} allows the internal engine to know that the cluster was empty.
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#' \itemize{
#' \item \strong{chunk_size}: How much data is loaded at once.
#' \item \strong{chunk_buffer}: Load chunks with a buffer.
#' \item \strong{chunk_alignment}: Align the chunks.
#' \item \strong{cores}: How many chunks are loaded and processed at once.
#' \item \strong{progress}: Displays a progress estimate.
#' \item \strong{output_files}: The user-function outputs will be written to files instead of being
#' returned into R.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files only if the user-defined function
#' returns a \code{LAS} object.
#' \item \strong{select}: Select only the data of interest to save processing memory.
#' \item \strong{filter}: Read only the points of interest.
#' }
#'
#' @examples
#' # Visit http://jean-romain.github.io/lidR/wiki for more examples
#'
#' ## =========================================================================
#' ## Example 1: detect all the tree tops over an entire catalog
#' ## (this is basically the existing lidR function 'tree_detection')
#' ## =========================================================================
#'
#' # 1. Build the user-defined function that analyzes each cluster of the catalog.
#' # The function's first argument is a LAScluster object. The other arguments can be freely
#' # choosen by the user.
#' my_tree_detection_method = function(cluster, ws)
#' {
#'   # The cluster argument is a LAScluster object. The user does not need to know how it works.
#'   # readLAS will load the region of interest (chunk) with a buffer around it, taking advantage of
#'   # point cloud indexation if possible. The filter and select options are propagated automatically
#'   las = readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'
#'   # Find the tree tops using a user-developed method (here simply a LMF).
#'   ttops = tree_detection(las, lmf(ws))
#'
#'   # ttops is a SpatialPointsDataFrame that contains the tree tops in our region of interest
#'   # plus the trees tops in the buffered area. We need to remove the buffer otherwise we will get
#'   # some trees more than once.
#'   bbox  <- raster::extent(cluster)
#'   ttops <- raster::crop(ttops, bbox)
#'
#'   return(ttops)
#' }
#'
#' # 2. Build a project (here, a single file catalog for the purposes of this dummmy example).
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' project <- catalog(LASfile)
#' plot(project)
#'
#' # 3. Set some catalog options
#' # For this dummy example, the clustering size is 80 m and the buffer is 10 m using a single core.
#' opt_chunk_buffer(project) <- 10
#' opt_cores(project) <- 1L
#' opt_chunk_size(project) <- 80        # extremely small because this is a dummy example
#' opt_select(project) <- "xyz"         # don't need to read something other than the coordinates
#' opt_filter(project) <- "-keep_first" # for this example we will use only first returns.
#'
#' # 4. Apply a user-defined function to take advantage of the internal engine
#' output = catalog_apply(project, my_tree_detection_method, ws = 5)
#'
#' # 5. Post-process the output to merge the results (depending on the output computed).
#' # Here, each value of the list is a SpatialPointsDataFrame, so rbind does the job:
#' output = do.call(sp::rbind, output)
#' sp::plot(output)
#'
#' ## ===================================================
#' ## Example 2: compute a rumple index on surface points
#' ## ===================================================
#'
#' rumple_index_surface = function(cluster, res)
#' {
#'   las = readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'
#'   las = lasfiltersurfacepoints(las, 0.5)
#'
#'   rumple <- grid_metrics(las, rumple_index(X,Y,Z), res)
#'   bbox   <- raster::extent(cluster)
#'   rumple <- raster::crop(rumple, bbox)
#'
#'   return(rumple)
#' }
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' project <- catalog(LASfile)
#'
#' opt_chunk_buffer(project) <- 1
#' opt_cores(project) <- 1L
#' opt_chunk_size(project) <- 80       # extremely small because this is a dummy example
#' opt_select(project) <- "xyz"        # don't need to read something other than the coordinates
#'
#' output = catalog_apply(project, rumple_index_surface, res = 20)
#' output = do.call(raster::merge, output)
#' plot(output, col = height.colors(50))
#' @export
catalog_apply <- function(ctg, FUN, ...)
{
  assertive::assert_is_all_of(ctg, "LAScatalog")
  assertive::assert_is_function(FUN)

  if (!check_fun_with_empty_cluster(FUN, ...))
    stop("User's function does not return NULL for empty chunks. Please see the documentation.")

  output <- catalog_apply2(ctg, FUN, ..., need_buffer = FALSE, check_alignement = FALSE, drop_null = TRUE, need_output_file = FALSE)
  return(output)
}

catalog_apply2 =  function(ctg, FUN, ..., need_buffer = FALSE, check_alignement = FALSE, drop_null = FALSE, need_output_file = FALSE, globals = TRUE)
{
  assertive::assert_is_function(FUN)
  assertive::assert_is_a_bool(need_buffer)
  assertive::assert_is_a_bool(check_alignement)
  assertive::assert_is_a_bool(drop_null)

  p          <- list(...)
  resolution <- if (is.null(p$res)) 0 else p$res
  start      <- if (is.null(p$start)) c(0,0) else p$start

  if (is(resolution, "RasterLayer"))
  {
    ext          <- raster::extent(resolution)
    resolution   <- raster::res(resolution)[1]
    start        <- c(ext@xmin, ext@ymin)
  }

  ctg      <- check_and_fix_options(ctg, need_buffer, check_alignement, need_output_file, res = resolution, start = start)
  clusters <- catalog_makecluster(ctg)
  clusters <- check_and_fix_clusters(ctg, clusters, check_alignement, res = resolution, start = start)

  oldstate <- options("lidR.progress")[[1]]
  options(lidR.progress = FALSE)

  output <- tryCatch(
  {
    cluster_apply(clusters, FUN, processing_options = ctg@processing_options, output_options = ctg@output_options, drop_null = drop_null, globals = globals, ...)
  },
  finally = {options(lidR.progress = oldstate)})

  return(output)
}

check_and_fix_options = function(ctg, need_buffer, check_alignement, need_output_file, res = NULL, start = NULL)
{
  # The function expect a buffer to guarantee a stric wall-to-wall output
  # (can be skipped if the catalog is not a wall-to-wall catalog)

  if (need_buffer & opt_chunk_buffer(ctg) <= 0 & opt_wall_to_wall(ctg))
    stop("A buffer greater than 0 is required to process the catalog. See help(\"LAScatalog-class\", \"lidR\")", call. = FALSE)

  # If we want to return a Raster*, to ensure a strict wall-to-wall output we need to check if the
  # clusters are aligned with the pixels. In case of chunk_size > 0 this is an easy to check before making
  # the clusters

  if (check_alignement & !opt_chunk_is_file(ctg) & opt_wall_to_wall(ctg))
  {
    # If the clustering option does not match with the resolution
    t_size     <- opt_chunk_size(ctg)
    new_t_size <- round_any(t_size, res)
    if (new_t_size != t_size)
    {
      opt_chunk_size(ctg) <- new_t_size
      message(glue::glue("Chunk size does not match with the resolution of the raster. Chunk size changed to {new_t_size} to ensure the continuity of the ouput."))
    }

    # If the alignment of the clusters does not match the start point of the raster
    alignment     <- opt_chunk_alignment(ctg)
    new_alignment <- (alignment - start) %% res + alignment
    if (any(new_alignment != alignment))
    {
      opt_chunk_alignment(ctg) <- new_alignment
      message(glue::glue("Alignment of the chunks do not match with the starting points of the raster. Alignment changed to ({new_alignment[1]}, {new_alignment[2]}) to ensure the continuity of the ouput."))
    }
  }

  # Some functions require outputs to be written in files because the output is likely to be too big to
  # be returned in R

  if (need_output_file & opt_output_files(ctg) == "")
    stop("This function requires that the LAScatalog provides an output file template. See  help(\"LAScatalog-class\", \"lidR\")", call. = FALSE)

  return(ctg)
}

check_and_fix_clusters = function(ctg, clusters, check_alignement, res = NULL, start = NULL)
{
  # If we want to return a Raster*, to ensure a strict wall-to-wall output we need to check if the
  # clusters are aligned with the pixels. In case of chunk_size =0 (processed by file) the clusters
  # must be checked after being created. Can be skipped if the catalog is not a wall-to-wall catalog.

  if (check_alignement & opt_chunk_is_file(ctg) & opt_wall_to_wall(ctg))
  {
    for (i in 1:length(clusters))
    {
      cluster <- clusters[[i]]
      bbox1 <- raster::extent(cluster)
      bbox2 <- bbox1
      xmin <- round_any(bbox1@xmin, res)
      ymin <- round_any(bbox1@ymin, res)
      xmax <- round_any(bbox1@xmax, res)
      ymax <- round_any(bbox1@ymax, res)
      bbox2@xmin <- if (xmin >= bbox1@xmin) xmin - res else xmin
      bbox2@ymin <- if (ymin >= bbox1@ymin) ymin - res else ymin
      bbox2@xmax <- if (xmax <= bbox1@xmax) xmax + res else xmax
      bbox2@ymax <- if (ymax <= bbox1@ymax) ymax + res else ymax

      if (!bbox1 == bbox2)
      {
        new_cluster      <- catalog_index(ctg, list(bbox2), LIDRRECTANGLE, opt_chunk_buffer(ctg))[[1]]
        new_cluster@save <- cluster@save
        clusters[[i]]    <- new_cluster
        #message(glue::glue("Chunk {i} has been slighly extended compared to the original file to ensure the continuity of the ouput."))
      }
    }
  }

  return(clusters)
}

check_fun_with_empty_cluster = function(FUN, ...)
{
  cl <- LAScluster(list(x = 0, y = 0), 0, 0, 0, LIDRRECTANGLE, system.file("extdata", "example.laz", package = "rlas"), "noname")
  cl@select <- "*"
  return(is.null(FUN(cl, ...)))
}
