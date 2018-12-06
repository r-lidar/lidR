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
#' This function gives users access to the \link[lidR:LAScatalog-class]{LAScatalog} processing engine.
#' It allows the application of a user-defined routine over an entire catalog. The LAScatalog
#' processing engine tool is explained in the \link[lidR:LAScatalog-class]{LAScatalog class}\cr\cr
#' \strong{Warning:} the LAScatalog processing engine has a mechanism to load buffered data to avoid
#' edge artifacts, but no mechanism to remove the buffer after applying user-defined functions, since
#' this task is specific to each process. In other \code{lidR} functions this task is performed
#' specifically for each function. In \code{catalog_apply} the user's function can return any output,
#' thus users must take care of this task themselves (See section "Edge artifacts")
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object.
#' @param FUN A user-defined function that respects a given template (see section function template)
#' @param ... Optional arguments to FUN.
#' @param .options See dedicated section and example.
#'
#' @section Edge artifacts:
#'
#' It is important to take precautions to avoid 'edge artifacts' when processing wall-to-wall
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process, or dummy elevations in a ground interpolation. The LAScatalog
#' processing engine provides internal tools to load buffered data. However, there is
#' no mechanism to remove the results computed in the buffered area since this task depends on the
#' output of the user-defined function. The user must take care of this task (see examples).
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
#' The parameter \code{FUN} expects a function with a first argument that will be supplied automatically
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
#'    # remove the buffer of the output
#'    return(something)
#' }}
#' The line \code{if(is.empty(las)) return(NULL)} is important because some clusters (chunks) may contain
#' 0 points (we can't know that before reading the file). In this case an empty point cloud with 0 points
#' is returned by \code{readLAS} and this may fail in subsequent code. Thus, exiting early from the user-defined
#' function by returning \code{NULL} indicates to the internal engine that the cluster was empty.
#'
#' @section .options:
#' User may have noticed that some lidR functions throw an errors when the processing options are innapropriated.
#' For example, some functions need a buffer and thus \code{buffer = 0} is forbidden. User can add the same
#' constrains to protect against innapropriated options. The \code{.options} argument can be a \code{list} of
#' options.
#' \itemize{
#' \item \code{need_buffer = TRUE} the function complains if the buffer is 0
#' \item \code{need_output_file = TRUE} the function complains if no output file template is provided
#' \item \code{drop_null = FALSE} the NULL outputs are not automatically removed (useful in very
#' specific internal cases)
#' \item \code{raster_alignment = ...} very important option, see below.
#' }
#'
#' When the function \code{FUN} returns a raster it is important to ensure that the chunks are aligned
#' with the raster to avoid egde artifacts. Indeed, if the edge of a chunk does not correspond to the edge
#' of the pixels the output will not be strictly continuous and will have egde artifacts (that might
#' not be visible). Users can check this with the options \code{raster_alignment}, that  can take the
#' resolution of the raster as input, as well as the starting point if needed. The folowing are accepted:\cr\cr
#' \preformatted{
#' # check if the chunks are aligned with a raster of resolution 20
#' raster_alignment = 20
#' raster_alignment = list(res = 20)
#'
#' # check if chunks are aligned with a raster of resolution 20
#' # that starts a (0,10)
#' raster_alignment = list(res = 20, start = c(0,10))
#' }
#' See also \link{grid_metrics} for more details.
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
#' \item \strong{output_files}: The user-defined function outputs will be written to files instead of being
#' returned into R.
#' \item \strong{laz_compression}: write \code{las} or \code{laz} files only if the user-defined function
#' returns a \code{LAS} object.
#' \item \strong{select}: Select only the data of interest to save processing memory.
#' \item \strong{filter}: Read only the points of interest.
#' }
#'
#' @examples
#' # More examples might be avaible in the official lidR vignettes or
#' # on the github wiki <http://jean-romain.github.io/lidR/wiki>
#'
#' ## =========================================================================
#' ## Example 1: detect all the tree tops over an entire catalog
#' ## (this is basically the reproduction of existing lidR function 'tree_detection')
#' ## =========================================================================
#'
#' # 1. Build the user-defined function that analyzes each chunk of the catalog.
#' # The function's first argument is a LAScluster object. The other arguments can be freely
#' # choosen by the user.
#' my_tree_detection_method <- function(cluster, ws)
#' {
#'   # The cluster argument is a LAScluster object. The user does not need to know how it works.
#'   # readLAS will load the region of interest (chunk) with a buffer around it, taking advantage of
#'   # point cloud indexation if possible. The filter and select options are propagated automatically
#'   las <- readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'
#'   # Find the tree tops using a user-developed method (here simply a LMF).
#'   ttops <- tree_detection(las, lmf(ws))
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
#' # 3. Set some catalog options.
#' # For this dummy example, the chunk size is 80 m and the buffer is 10 m using a single core.
#' opt_chunk_buffer(project) <- 10
#' opt_cores(project)        <- 1L
#' opt_chunk_size(project)   <- 80            # small because this is a dummy example.
#' opt_select(project)       <- "xyz"         # read only the coordinates.
#' opt_filter(project)       <- "-keep_first" # read only first returns.
#'
#' # 4. Apply a user-defined function to take advantage of the internal engine
#' opt    <- list(need_buffer = TRUE)   # catalog_apply will throw an error if buffer = 0
#' output <- catalog_apply(project, my_tree_detection_method, ws = 5, .options = opt)
#'
#' # 5. Post-process the output to merge the results (depending on the output computed).
#' # Here, each value of the list is a SpatialPointsDataFrame, so rbind does the job:
#' output <- do.call(rbind, output)
#' spplot(output)
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
#'   las    <- lasfiltersurfacepoints(las, 1)
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
#' opt_cores(project)        <- 1L
#' opt_chunk_size(project)   <- 120     # small because this is a dummy example.
#' opt_select(project)       <- "xyz"   # read only the coordinates.
#'
#' opt     <- list(raster_alignment = 20)  # catalog_apply will adjust the chunks if required
#' output  <-  catalog_apply(project, rumple_index_surface, res = 20, .options = opt)
#' output  <- do.call(raster::merge, output)
#' plot(output, col = height.colors(50))
#' @export
catalog_apply <- function(ctg, FUN, ..., .options = NULL)
{
  assert_is_all_of(ctg, "LAScatalog")
  assert_is_function(FUN)
  assert_FUN_is_NULL_with_empty_cluster(ctg, FUN, ...)

  options          <- parse_options(.options)
  raster_alignment <- options$raster_alignment
  need_buffer      <- options$need_buffer
  check_alignment  <- options$check_alignment
  drop_null        <- options$drop_null
  need_output_file <- options$need_output_file
  globals          <- options$globals

  resolution       <- raster_alignment$res
  start            <- raster_alignment$start

  ctg              <- check_and_fix_options(ctg, need_buffer, check_alignment, need_output_file, resolution, start)
  clusters         <- catalog_makecluster(ctg)
  clusters         <- check_and_fix_clusters(ctg, clusters, check_alignment, resolution, start)

  oldstate         <- options("lidR.progress")[[1]]
  options(lidR.progress = FALSE)

  output <- tryCatch(
  {
    cluster_apply(clusters, FUN, processing_options = ctg@processing_options, output_options = ctg@output_options, drop_null = drop_null, globals = globals, ...)
  },
  finally = {options(lidR.progress = oldstate)})

  return(output)
}

check_and_fix_options = function(ctg, need_buffer, check_alignment, need_output_file, res, start)
{
  # The function expects a buffer to guarantee a strict wall-to-wall output

  if (need_buffer & opt_chunk_buffer(ctg) <= 0 & opt_wall_to_wall(ctg))
    stop("A buffer greater than 0 is required to process the catalog.")

  # The function require outputs to be written in files (because the output is likely to be too big to be returned in R)

  if (need_output_file & opt_output_files(ctg) == "")
    stop("This function requires that the LAScatalog provides an output file template.")

  # The function requires that the chunks are aligned with a raster (typically the function returns a Raster*).
  # To ensure a strict wall-to-wall output, check if the chunks are aligned with the pixels. In case
  # of chunk_size > 0 this is easy to check before making the clusters

  if (check_alignment & !opt_chunk_is_file(ctg) & opt_wall_to_wall(ctg))
  {
    # If the chunk_size option does not match with the resolution
    chunk_size     <- opt_chunk_size(ctg)
    new_chunk_size <- round_any(chunk_size, res)

    if (new_chunk_size != chunk_size)
    {
      opt_chunk_size(ctg) <- new_chunk_size
      message(glue::glue("Chunk size does not match with the resolution of the raster. Chunk size changed to {new_chunk_size} to ensure the continuity of the output."))
    }

    # If the alignment of the chunks does not match the start point of the raster
    chunk_alignment     <- opt_chunk_alignment(ctg)
    new_chunk_alignment <- abs((chunk_alignment - start)) %% res + chunk_alignment
    if (any(new_chunk_alignment != chunk_alignment))
    {
      opt_chunk_alignment(ctg) <- new_chunk_alignment
      message(glue::glue("Alignment of the chunks does not match with the starting points of the raster. Alignment changed to ({new_chunk_alignment[1]}, {new_chunk_alignment[2]}) to ensure the continuity of the output."))
    }
  }

  return(ctg)
}

check_and_fix_clusters = function(ctg, clusters, check_alignment, res, start)
{
  # The function requires that the chunks are aligned with a raster (typically the function returns a Raster*).
  # In case of chunk_size = 0 (processed by file) the chunks must be checked after being created..

  if (check_alignment & opt_chunk_is_file(ctg) & opt_wall_to_wall(ctg))
  {
    for (i in 1:length(clusters))
    {
      cluster    <- clusters[[i]]
      bbox1      <- raster::extent(cluster)
      bbox2      <- bbox1
      xmin       <- round_any(bbox1@xmin, res)
      ymin       <- round_any(bbox1@ymin, res)
      xmax       <- round_any(bbox1@xmax, res)
      ymax       <- round_any(bbox1@ymax, res)
      bbox2@xmin <- if (xmin >= bbox1@xmin) xmin - res else xmin
      bbox2@ymin <- if (ymin >= bbox1@ymin) ymin - res else ymin
      bbox2@xmax <- if (xmax <= bbox1@xmax) xmax + res else xmax
      bbox2@ymax <- if (ymax <= bbox1@ymax) ymax + res else ymax

      if (!bbox1 == bbox2)
      {
        new_cluster      <- catalog_index(ctg, list(bbox2), LIDRRECTANGLE, opt_chunk_buffer(ctg))[[1]]
        new_cluster@save <- cluster@save
        clusters[[i]]    <- new_cluster
        #message(glue::glue("Chunk {i} has been slighly extended compared to the original file to ensure the continuity of the output."))
      }
    }
  }

  return(clusters)
}

assert_FUN_is_NULL_with_empty_cluster = function(ctg, FUN, ...)
{
  if (opt_wall_to_wall(ctg) == TRUE)
  {
    cl <- LAScluster(list(x = 0, y = 0), 0, 0, 0, LIDRRECTANGLE, system.file("extdata", "example.laz", package = "rlas"), "noname")
    cl@select <- "*"

    if (!is.null(FUN(cl, ...)))
      stop("User's function does not return NULL for empty chunks. Please see the documentation.")
  }
}

parse_options = function(.option)
{
  output <- list()

  raster_alignment <- .option$raster_alignment

  if (is.null(raster_alignment))
  {
    raster_alignment <- NULL
    check_alignment  <- FALSE
  }
  else if (is.numeric(raster_alignment))
  {
    assert_is_a_number(raster_alignment)
    assert_all_are_non_negative(raster_alignment)
    raster_alignment <- list(res = raster_alignment, start = c(0,0))
    check_alignment <- TRUE
  }
  else if (is.list(raster_alignment))
  {
    assert_is_a_number(raster_alignment$res)
    assert_all_are_non_negative(raster_alignment$res)
    check_alignment <- TRUE

    if (!is.null(raster_alignment$start))
      assert_is_numeric(raster_alignment$start)
  }
  else
    stop("Invalid parameter 'raster_alignment")

  output$raster_alignment <- raster_alignment
  output$check_alignment  <- check_alignment

  if (is.null(.option$need_output_file))
  {
    need_output_file <- FALSE
  }
  else
  {
    need_output_file <- .option$need_output_file
    assert_is_a_bool(need_output_file)
  }

  output$need_output_file <- need_output_file

  if (is.null(.option$drop_null))
  {
    drop_null <- TRUE
  }
  else
  {
    drop_null <- .option$drop_null
    assert_is_a_bool(drop_null)
  }

  output$drop_null <- drop_null

  if (is.null(.option$need_buffer))
  {
    need_buffer <- FALSE
  }
  else
  {
    need_buffer <- .option$need_buffer
    assert_is_a_bool(need_buffer)
  }

  output$need_buffer <- need_buffer

  if (is.null(.option$globals))
    output$globals <- NULL
  else
    output$globals <- .option$globals

  return(output)
}
