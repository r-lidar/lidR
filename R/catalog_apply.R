#' LAScatalog processing engine
#'
#' This function gives users access to the \link[=LAScatalog-class]{LAScatalog} processing engine.
#' It allows the application of a user-defined routine over a collection of LAS/LAZ files. The
#' LAScatalog processing engine is explained in the \link[=LAScatalog-class]{LAScatalog class}\cr\cr
#' `catalog_apply()` is the core of the lidR package. It drives every single function that can process a
#' \code{LAScatalog}. It is flexible and powerful but also complex. `catalog_map()` is a simplified version
#' of `catalog_apply()` that suits for 90% of use cases.\cr\cr
#' `catalog_sapply()` is a previous attempt to provide simplified version of `catalog_apply()`. Use
#' `catalog_map()` instead.
#'
#' @param ctg A \link[=LAScatalog-class]{LAScatalog} object.
#' @param FUN A user-defined function that respects a given template (see section function template)
#' @param ... Optional arguments to FUN.
#' @param .options See dedicated section and examples.
#'
#' @section Edge artifacts:
#'
#' It is important to take precautions to avoid 'edge artifacts' when processing wall-to-wall
#' tiles. If the points from neighbouring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process, or dummy elevations in a ground interpolation. The LAScatalog
#' processing engine provides internal tools to load buffered data 'on-the-fly'. `catalog_map()` takes
#' care of removing automatically the results computed in the buffered area to avoid unexpected output
#' with duplicated entries or conflict between values computed twice. It does that in predefined way
#' that may not suit all cases. `catalog_apply()` does not remove the buffer and leave users free
#' to handle this in a custom way. This is why `catalog_apply()` is more complex but gives more freedom
#' to build new applications.
#'
#' @section Buffered data:
#'
#' The LAS objects loaded in theses functions have a special attribute called 'buffer' that indicates,
#' for each point, if it comes from a buffered area or not. Points from non-buffered areas have a
#' 'buffer' value of 0, while points from buffered areas have a 'buffer' value of 1, 2, 3 or 4, where
#' 1 is the bottom buffer and 2, 3 and 4 are the left, top and right buffers, respectively. This allows
#' for filtering of buffer points if required.
#'
#' @section Function template:
#'
#' The parameter `FUN` of `catalog_apply` expects a function with a first argument that will be
#' supplied automatically by the `LAScatalog` processing engine. This first argument is a `LAScluster`.
#' A `LAScluster` is an internal undocumented class but the user needs to know only three things about
#' this class:
#'
#' - It represents a chunk of the file collection
#' - The function \link{readLAS} can be used with a `LAScluster`
#' - The function \link[raster:extent]{extent} or \link[sf:st_bbox]{st_bbox}
#' can be used with a `LAScluster` and they return the bounding box of the chunk without the buffer.
#' It must be used to clip the output and remove the buffered region (see examples).
#'
#' A user-defined function must be templated like this:
#'
#' ```
#' myfun <- function(chunk, ...) {
#'    # Load the chunk + buffer
#'    las <- readLAS(chunk)
#'    if (is.empty(las)) return(NULL)
#'
#'    # do something
#'    output <- do_something(las, ...)
#'
#'    # remove the buffer of the output
#'    bbox <- bbox(chunk)
#'    output <- remove_buffer(output, bbox)
#'    return(output)
#' }
#' ```
#'
#' The line `if (is.empty(las)) return(NULL)` is important because some clusters (chunks) may contain
#' 0 points (we can't know this before reading the file). In this case an empty point cloud with 0 points
#' is returned by `readLAS()` and this may fail in subsequent code. Thus, exiting early from the user-defined
#' function by returning `NULL` indicates to the internal engine that the chunk was empty.
#'
#' `catalog_map` is much simpler (but less versatile). It automatically takes care of reading the chunk
#' and checks if the point cloud is empty. It also automatically crop the buffer. The way it crops the
#' buffer suits for most cases but for some special cases it may be advised to handle this in a more
#' specific way i.e. using `catalog_apply()`. For `catalog_map()` the first argument is a `LAS` and the
#' template is:
#'
#' ```
#' myfun <- function(las, ...) {
#'    # do something
#'    output <- do_something(las, ...)
#'    return(output)
#' }
#' ```
#'
#' @section .options:
#' Users may have noticed that some lidR functions throw an error when the processing options are
#' inappropriate. For example, some functions need a buffer and thus `buffer = 0` is forbidden.
#' Users can add the same constraints to protect against inappropriate options. The `.options`
#' argument is a `list` that allows users to tune the behaviour of the processing engine.
#'
#' - `drop_null = FALSE`: not intended to be used by regular users. The engine does not remove
#' NULL outputs
#' - `need_buffer = TRUE`: the function complains if the buffer is 0.
#' - `need_output_file = TRUE` the function complains if no output file template is provided.
#' - `raster_alignment = ...` the function checks the alignment of the chunks. This option is
#' important if the output is a raster. See below for more details.
#' - `automerge = TRUE` by default the engine returns a `list` with one item per chunk. If
#' `automerge = TRUE`, it tries to merge the outputs into a single object: a `Raster*`, a
#' `Spatial*`, a `LAS*`, an `sf`, a ` stars` similarly to other functions of the package. This is a
#'  fail-safe option so in the worst case, if the merging fails, the `list` is returned. This is
#'  activated by `catalog_map` making it simpler.
#' - `autoread = TRUE`. Introduced in v3.0.0 this option enables to get rid of the first steps of the
#' function i.e `readLAS()` and `if (is.empty())`. In this case the function must take two
#' objects as input, first a `LAS` object and second a `bbox` from `sf`. This is
#' activated by `catalog_map` making it simpler.
#' - `autocrop = TRUE`. Introduced in v4.0.0 this option enables to get rid of the buffer crop steps
#' of the function i.e `something <- remove_buffer(something, bbox)`. In this case the function must
#' take one `LAS` object as input. This is activated by `catalog_map` making it simpler.
#'
#' When the function `FUN` returns a raster it is important to ensure that the chunks are aligned
#' with the raster to avoid edge artifacts. Indeed, if the edge of a chunk does not correspond to the edge
#' of the pixels, the output will not be strictly continuous and will have edge artifacts (that might
#' not be visible). Users can check this with the options `raster_alignment`, that can take the
#' resolution of the raster as input, as well as the starting point if needed. The following are accepted:
#'
#' ```
#' # check if chunks are aligned with a raster of resolution 20
#' raster_alignment = 20
#' raster_alignment = list(res = 20)
#'
#' # check if chunks are aligned with a raster of resolution 20
#' # that starts at (0,10)
#' raster_alignment = list(res = 20, start = c(0,10))
#' ```
#'
#' @examples
#' # More examples might be avaible in the official lidR vignettes or
#' # on the github book <https://jean-romain.github.io/lidRbook/>
#'
#' ## =========================================================================
#' ## Example 1: detect all the tree tops over an entire catalog
#' ## (this is basically a reproduction of the existing function 'locate_trees')
#' ## =========================================================================
#'
#' # 1. Build the user-defined function that analyzes each chunk of the catalog.
#' # The function's first argument is a LAScluster object. The other arguments can be freely
#' # chosen by the users.
#' my_tree_detection_method <- function(chunk, ws)
#' {
#'   # The chunk argument is a LAScluster object. The users do not need to know how it works.
#'   # readLAS will load the region of interest (chunk) with a buffer around it, taking advantage of
#'   # point cloud indexation if possible. The filter and select options are propagated automatically
#'   las <- readLAS(chunk)
#'   if (is.empty(las)) return(NULL)
#'
#'   # Find the tree tops using a user-developed method
#'   # (here simply a LMF for the example).
#'   ttops <- locate_trees(las, lmf(ws))
#'
#'   # ttops is an sf object that contains the tree tops in our region of interest
#'   # plus the trees tops in the buffered area. We need to remove the buffer otherwise we will get
#'   # some trees more than once.
#'   bbox  <- st_bbox(chunk)
#'   ttops <- sf::st_crop(ttops, bbox)
#'   return(ttops)
#' }
#'
#' # 2. Build a collection of file
#' # (here, a single file LAScatalog for the purposes of this simple example).
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile)
#' plot(ctg)
#'
#' # 3. Set some processing options.
#' # For this small single file example, the chunk size is 100 m + 10 m of buffer
#' opt_chunk_buffer(ctg) <- 10
#' opt_chunk_size(ctg)   <- 100            # Small because this is a dummy example.
#' opt_chunk_alignment(ctg) <- c(-50, -35) # Align such as it creates 2 chunks only.
#' opt_select(ctg)       <- "xyz"          # Read only the coordinates.
#' opt_filter(ctg)       <- "-keep_first"  # Read only first returns.
#'
#' # 4. Apply a user-defined function to take advantage of the internal engine
#' opt    <- list(need_buffer = TRUE,   # catalog_apply will throw an error if buffer = 0
#'                automerge   = TRUE)   # catalog_apply will merge the outputs into a single object
#' output <- catalog_apply(ctg, my_tree_detection_method, ws = 5, .options = opt)
#'
#' plot(output)
#'
#' \donttest{
#' ## =========================================================================
#' ## Example 1: simplified. There is nothing that requires special data
#' ## manipulation in the previous example. Everything can be handled automatically
#' ##=========================================================================
#'
#' # 1. Build the user-defined function that analyzes a point cloud.
#' my_tree_detection_method <- function(las, ws)
#' {
#'   # Find the tree tops using a user-developed method
#'   # (here simply a LMF for the example).
#'   ttops <- locate_trees(las, lmf(ws))
#'   return(ttops)
#' }
#'
#' # 2. Build a project
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile)
#' plot(ctg)
#'
#' # 3. Set some processing options.
#' # For this dummy example, the chunk size is 100 m and the buffer is 10 m
#' opt_chunk_buffer(ctg) <- 10
#' opt_chunk_size(ctg)   <- 100            # small because this is a dummy example.
#' opt_chunk_alignment(ctg) <- c(-50, -35) # Align such as it creates 2 chunks only.
#' opt_select(ctg)       <- "xyz"          # Read only the coordinates.
#' opt_filter(ctg)       <- "-keep_first"  # Read only first returns.
#'
#' # 4. Apply a user-defined function to take advantage of the internal engine
#' opt    <- list(need_buffer = TRUE)   # catalog_apply will throw an error if buffer = 0
#' output <- catalog_map(ctg, my_tree_detection_method, ws = 5, .options = opt)
#' }
#'
#' \dontrun{
#' ## ===================================================
#' ## Example 2: compute a rumple index on surface points
#' ## ===================================================
#'
#' rumple_index_surface = function(las, res)
#' {
#'   las    <- filter_surfacepoints(las, 1)
#'   rumple <- pixel_metrics(las, ~rumple_index(X,Y,Z), res)
#'   return(rumple)
#' }
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg <- readLAScatalog(LASfile)
#'
#' opt_chunk_buffer(ctg) <- 1
#' opt_chunk_size(ctg)   <- 140     # small because this is a dummy example.
#' opt_select(ctg)       <- "xyz"   # read only the coordinates.
#'
#' opt    <- list(raster_alignment = 20) # catalog_apply will adjust the chunks if required
#' output <- catalog_map(ctg, rumple_index_surface, res = 20, .options = opt)
#'
#' plot(output, col = height.colors(25))
#' }
#' @export
#' @md
catalog_apply <- function(ctg, FUN, ..., .options = NULL)
{
  # Assert correctness of inputs
  assert_is_all_of(ctg, "LAScatalog")
  assert_is_function(FUN)

  # Parse options to check validity or initialize some missing
  opt <- engine_parse_options(.options)

  # Assert correctness and check alignment
  if (opt[["autoread"]] == FALSE) assert_fun_is_null_with_empty_cluster(ctg, FUN, ...)
  assert_processing_constraints_are_repected(ctg, opt[["need_buffer"]], opt[["need_output_file"]])

  # Realignment of the chunk if needed
  realigment <- if (opt[["check_alignment"]]) realigment = opt[["raster_alignment"]] else FALSE

  # Produce the chunks
  clusters <- engine_chunks(ctg, realigment)

  # Disable the progress bar of the functions and ensure user options are restored
  oldstate <- getOption("lidR.progress")
  options(lidR.progress = FALSE)
  on.exit(options(lidR.progress = oldstate), add = TRUE)

  # Process with the catalog processing engine
  output <- engine_apply(clusters, FUN, ctg@processing_options, ctg@output_options, opt[["globals"]], opt[["autoread"]], opt[["autocrop"]], ...)

  # Filter NULLs and return
  if (isTRUE(opt[["drop_null"]]))
    output <- Filter(Negate(is.null), output)

  # Automerge
  if (!isFALSE(opt[["automerge"]]) && opt_merge(ctg))
    output <- engine_merge(ctg, output, as.character(substitute(FUN)))

  return(output)
}

#' @export
#' @rdname catalog_apply
catalog_sapply <- function(ctg, FUN, ..., .options = NULL)
{
  if (is.null(.options))
    .options <- list(automerge = TRUE)
  else
  {
    .options$automerge <- TRUE
  }

  return(catalog_apply(ctg, FUN, ..., .options = .options))
}

#' @export
#' @rdname catalog_apply
catalog_map <- function(ctg, FUN, ..., .options = NULL)
{
  if (is.null(.options))
    .options <- list(automerge = TRUE,
                     autocrop  = TRUE,
                     autoread  = TRUE)
  else
  {
    .options$automerge <- TRUE
    .options$autocrop  <- TRUE
    .options$autoread  <- TRUE
  }

  return(catalog_apply(ctg, FUN, ..., .options = .options))
}

assert_fun_is_null_with_empty_cluster = function(ctg, FUN, ...)
{
  if (opt_wall_to_wall(ctg) == TRUE)
  {
    cl <- LAScluster(list(x = 0, y = 0), 0, 0, 0, LIDRRECTANGLE, system.file("extdata", "example.laz", package = "rlas"), "noname")
    cl@select <- "*"

    u <- tryCatch(FUN(cl, ...), error = function(e)
    {
      stop(paste0("The function is tested before starting processing the collection and failed with following error:\n", e), call. = FALSE)
    })

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

  # autocrop

  if (is.null(.option$autocrop))
  {
    autocrop <- FALSE
  }
  else
  {
    autocrop <- .option$autocrop

    if (isTRUE(autocrop))
      autocrop <- TRUE
  }

  output$autocrop <- autocrop

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
