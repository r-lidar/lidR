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
#' `catalog_apply()` is the core of the lidR package. It drives every single function that can process a
#' \code{LAScatalog}. It is flexible and powerful but also complex. `catalog_sapply` is the same
#' with the option `automerge = TRUE` enforced to simplify the output.\cr\cr
#' **Warning:** the LAScatalog processing engine has a mechanism to load buffered data 'on-the-fly'
#' to avoid edge artifacts, but no mechanism to remove the buffer after applying user-defined functions,
#' since this task is specific to each process. In other `lidR` functions this task is performed
#' specifically for each function. In `catalog_apply()` the user's function can return any output,
#' thus users must take care of this task themselves (See section "Edge artifacts")
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object.
#' @param FUN A user-defined function that respects a given template (see section function template)
#' @param ... Optional arguments to FUN.
#' @param .options See dedicated section and examples.
#'
#' @section Edge artifacts:
#'
#' It is important to take precautions to avoid 'edge artifacts' when processing wall-to-wall
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process, or dummy elevations in a ground interpolation. The LAScatalog
#' processing engine provides internal tools to load buffered data 'on-the-fly'. However, there is
#' no mechanism to remove the results computed in the buffered area since this task depends on the
#' output of the user-defined function. The user must take care of this task (see examples) to prevent
#' unexpected output with duplicated entries or conflict between values computed twice.
#'
#' @section Buffered data:
#'
#' The LAS objects read by the user function have a special attribute called 'buffer' that indicates,
#' for each point, if it comes from a buffered area or not. Points from non-buffered areas have a
#' 'buffer' value of 0, while points from buffered areas have a 'buffer' value of 1, 2, 3 or 4, where
#' 1 is the bottom buffer and 2, 3 and 4 are the left, top and right buffers, respectively. This allows
#' for filtering of buffer points if required.
#'
#' @section Function template:
#'
#' The parameter `FUN` expects a function with a first argument that will be supplied automatically
#' by the `LAScatalog` processing engine. This first argument is a `LAScluster`. A `LAScluster`
#' is an internal undocumented class but the user needs to know only three things about this class:
#'
#' - It represents a chunk of the catalog
#' - The function \link{readLAS} can be used with a `LAScluster`
#' - The function \link[raster:extent]{extent} or \link[sp:bbox]{bbox} can be used with a `LAScluster`
#' and it returns the bounding box of the cluster without the buffer. It can be used to clip the output
#' and remove the buffered region (see examples).
#'
#' A user-defined function must be templated like this:
#'
#' ```
#' myfun <- function(cluster, ...) {
#'    las <- readLAS(cluster)
#'    if (is.empty(las)) return(NULL)
#'    # do something
#'    # remove the buffer of the output
#'    return(something)
#' }
#' ```
#'
#' The line `if(is.empty(las)) return(NULL)` is important because some clusters (chunks) may contain
#' 0 points (we can't know this before reading the file). In this case an empty point cloud with 0 points
#' is returned by `readLAS()` and this may fail in subsequent code. Thus, exiting early from the user-defined
#' function by returning `NULL` indicates to the internal engine that the cluster was empty.
#'
#' From v3.0.0 if `autoread = TRUE` the following template is accepted because the engine takes care
#' of the above mentionned steps:
#'
#' ```
#' myfun <- function(las, bbox ...) {
#'    # do something
#' }
#' ````
#'
#' @section .options:
#' Users may have noticed that some lidR functions throw an error when the processing options are
#' inappropriate. For example, some functions need a buffer and thus `buffer = 0` is forbidden.
#' Users can add the same constraints to protect against inappropriate options. The `.options`
#' argument is a `list` that allows users to tune the behavior of the processing engine.
#'
#' - `drop_null = FALSE` Not intended to be used by regular users. The engine does not remove
#' NULL outputs
#' - `need_buffer = TRUE` the function complains if the buffer is 0.
#' - `need_output_file = TRUE` the function complains if no output file template is provided.
#' - `raster_alignment = ...` the function checks the alignment of the chunks. This option is
#' important if the output is a raster. See below for more details.
#' - `automerge = TRUE` by defaut the engine returns a `list`` with one item per chunk. If
#' `automerge = TRUE`, it tries to merge the outputs into a single object: a `Raster*``, a
#' `Spatial*`, a `LAS*` similar to other functions of the package. This is a fail-safe
#' option so in the worst case, if the merge fails, the `list` is returned.
#' - `autoread = TRUE`. Introduced in v3.0.0 this option enables to get rid of the first steps of the
#' function i.e `readLAS()` and `if (is.empty())`. In this case the function must take two
#' objects as input, first a `LAS` object and second a `Extent` from `raster`.
#'
#' When the function `FUN` returns a raster it is important to ensure that the chunks are aligned
#' with the raster to avoid edge artifacts. Indeed, if the edge of a chunk does not correspond to the edge
#' of the pixels, the output will not be strictly continuous and will have edge artifacts (that might
#' not be visible). Users can check this with the options `raster_alignment`, that can take the
#' resolution of the raster as input, as well as the starting point if needed. The following are accepted:
#'
#' ````
#' # check if chunks are aligned with a raster of resolution 20
#' raster_alignment = 20
#' raster_alignment = list(res = 20)
#'
#' # check if chunks are aligned with a raster of resolution 20
#' # that starts at (0,10)
#' raster_alignment = list(res = 20, start = c(0,10))
#' ```
#'
#' See also \link{grid_metrics} for more details.
#'
#' @section Supported processing options:
#' Supported processing options for a \code{LAScatalog} (in bold). For more details see the
#' \link[lidR:LAScatalog-class]{LAScatalog engine documentation}:
#'
#' - *chunk_size*: How much data is loaded at once.
#' - *chunk_buffer*: Load chunks with a buffer.
#' - *chunk_alignment*: Align the chunks.
#' - *progress*: Displays a progress estimate.
#' - *output_files*: The user-defined function outputs will be written to files instead of being
#' returned into R.
#' - *laz_compression*: write `las` or `laz` files only if the user-defined function
#' returns a `LAS`` object.
#' - *select*: Select only the data of interest to save processing memory.
#' - *filter*: Read only the points of interest.
#'
#' @examples
#' # More examples might be avaible in the official lidR vignettes or
#' # on the github wiki <http://jean-romain.github.io/lidR/wiki>
#'
#' ## =========================================================================
#' ## Example 1: detect all the tree tops over an entire catalog
#' ## (this is basically a reproduction of the existing lidR function 'tree_detection')
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
#' project <- readLAScatalog(LASfile)
#' plot(project)
#'
#' # 3. Set some catalog options.
#' # For this dummy example, the chunk size is 80 m and the buffer is 10 m
#' opt_chunk_buffer(project) <- 10
#' opt_chunk_size(project)   <- 80            # small because this is a dummy example.
#' opt_select(project)       <- "xyz"         # read only the coordinates.
#' opt_filter(project)       <- "-keep_first" # read only first returns.
#'
#' # 4. Apply a user-defined function to take advantage of the internal engine
#' opt    <- list(need_buffer = TRUE,   # catalog_apply will throw an error if buffer = 0
#'                automerge   = TRUE)   # catalog_apply will merge the outputs into a single object
#' output <- catalog_apply(project, my_tree_detection_method, ws = 5, .options = opt)
#'
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
#'   las    <- filter_surfacepoints(las, 1)
#'   rumple <- grid_metrics(las, ~rumple_index(X,Y,Z), res)
#'   bbox   <- raster::extent(cluster)
#'   rumple <- raster::crop(rumple, bbox)
#'
#'   return(rumple)
#' }
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' project <- readLAScatalog(LASfile)
#'
#' opt_chunk_buffer(project) <- 1
#' opt_chunk_size(project)   <- 120     # small because this is a dummy example.
#' opt_select(project)       <- "xyz"   # read only the coordinates.
#'
#' opt     <- list(raster_alignment = 20, # catalog_apply will adjust the chunks if required
#'                 automerge = TRUE)      # catalog_apply will merge the outputs into a single raster
#' output  <- catalog_apply(project, rumple_index_surface, res = 20, .options = opt)
#'
#' plot(output, col = height.colors(50))
#' @export
#' @md
catalog_apply <- function(ctg, FUN, ..., .options = NULL)
{
  # ==== INITIALISATONS ====

  # Assert correctness of inputs
  assert_is_all_of(ctg, "LAScatalog")
  assert_is_function(FUN)

  # Store stuff in 3-letter variables to reduce width of next lines
  opt <- engine_parse_options(.options)
  ral <- opt[["raster_alignment"]]
  nbu <- opt[["need_buffer"]]
  mer <- opt[["automerge"]]
  rea <- opt[["autoread"]]
  cal <- opt[["check_alignment"]]
  dnu <- opt[["drop_null"]]
  nof <- opt[["need_output_file"]]
  glo <- opt[["globals"]]
  res <- ral[["res"]]
  sta <- ral[["start"]]

  pop <- ctg@processing_options
  oop <- ctg@output_options

  # Assert correctness and check aligment
  if (rea == FALSE) assert_fun_is_null_with_empty_cluster(ctg, FUN, ...)
  assert_processing_constraints_are_repected(ctg, nbu, nof)

  # Realignment
  realigment = FALSE
  if (cal) realigment = list(res = res, start = sta)

  # Produce the chunks
  clusters <- catalog_makecluster(ctg, realigment)

  # Disable the progress bar of the functions but ensure user options are restored
  oldstate <- getOption("lidR.progress")
  options(lidR.progress = FALSE)
  on.exit(options(lidR.progress = oldstate), add = TRUE)

  # Process
  output <- cluster_apply(clusters, FUN, pop, oop, glo, rea, ...)

  # Filter NULLs and return
  if (isTRUE(dnu))
    output <- Filter(Negate(is.null), output)

  # Automerge
  if (!isFALSE(mer) && opt_merge(ctg))
    output <- catalog_merge_results(ctg, output, mer, as.character(substitute(FUN)))

  return(output)
}

#' @export
#' @rdname catalog_apply
catalog_sapply <- function(ctg, FUN, ..., .options = NULL)
{
  if (is.null(.options))
    .options <- list(automerge = TRUE)
  else
    .options$automerge <- TRUE

  return(catalog_apply(ctg, FUN, ..., .options = .options))
}

assert_fun_is_null_with_empty_cluster = function(ctg, FUN, ...)
{
  if (opt_wall_to_wall(ctg) == TRUE)
  {
    cl <- LAScluster(list(x = 0, y = 0), 0, 0, 0, LIDRRECTANGLE, system.file("extdata", "example.laz", package = "rlas"), "noname")
    cl@select <- "*"

    u <- tryCatch(FUN(cl, ...), error = function(e) {return(0)})

    if (!is.null(u))
      stop("User's function does not return NULL for empty chunks. Please see the documentation of catalog_apply.", call. = FALSE)
  }
}

assert_processing_constraints_are_repected <- function(ctg, need_buffer, need_output_file)
{
  # The function expects a buffer to guarantee a strict wall-to-wall output
  if (need_buffer & opt_chunk_buffer(ctg) <= 0 && opt_wall_to_wall(ctg))
    stop("A buffer greater than 0 is required to process the catalog.", call. = FALSE)

  # The function requires the output file template to be written in files
  # (because the output is likely to be too big to be returned in R)
  if (need_output_file & opt_output_files(ctg) == "")
    stop("This function requires that the LAScatalog provides an output file template.", call. = FALSE)
}

engine_parse_options = function(.option)
{
  output <- list()

  # Alignment

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

  # output file

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

  # drop null

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

  # automerge

  if (is.null(.option$automerge))
  {
    automerge <- FALSE
  }
  else
  {
    automerge <- .option$automerge

    if (isTRUE(automerge))
      automerge <- "auto"
  }

  output$automerge <- automerge

  # autoread

  if (is.null(.option$autoread))
  {
    autoread <- FALSE
  }
  else
  {
    autoread <- .option$autoread

    if (isTRUE(autoread))
      autoread <- TRUE
  }

  output$autoread <- autoread

  # need buffer

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

  # export globals

  if (is.null(.option$globals))
    output$globals <- NULL
  else
    output$globals <- .option$globals

  return(output)
}
