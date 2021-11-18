#' Get or set LAScatalog processing engine options
#'
#' The names of the options and their roles are documented in \link[=LAScatalog-class]{LAScatalog}.
#' The options are used by all the functions that support a `LAScatalog` as input. Most options are
#' easy to understand and to link to the documentation of \link[=LAScatalog-class]{LAScatalog} but some
#' need more details. See section 'Details'.
#'
#' - **opt_restart()** automatically sets the chunk option named "drop" in such a way that
#' the engine will restart at a given chunk and skip all previous chunks. Useful for restarting after a crash.
#' - **opt_independent_file()** automatically sets the chunk size, chunk buffer and wall-to-wall options
#' to process files that are not spatially related to each other, such as plot inventories.
#' - **opt_laz_compression()** automatically modifies the drivers to write LAZ files instead of LAS files.
#'
#' @param ctg An object of class \link[=LAScatalog-class]{LAScatalog}
#' @param value An appropriate value depending on the expected input.
#'
#' @name engine_options
#' @rdname engine_options
#' @family LAScatalog processing engine
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg = readLAScatalog(LASfile)
#'
#' plot(ctg, chunk_pattern = TRUE)
#'
#' opt_chunk_size(ctg) <- 150
#' plot(ctg, chunk_pattern = TRUE)
#'
#' opt_chunk_buffer(ctg) <- 10
#' plot(ctg, chunk_pattern = TRUE)
#'
#' opt_chunk_alignment(ctg) <- c(270,250)
#' plot(ctg, chunk_pattern = TRUE)
#'
#' summary(ctg)
#'
#' opt_output_files(ctg) <- "/path/to/folder/templated_filename_{XBOTTOM}_{ID}"
#' summary(ctg)
#' @md
NULL

# ========= Chunk Options ===============

#' @rdname engine_options
#' @export
opt_chunk_buffer = function(ctg)
{
  return(ctg@chunk_options$buffer)
}

#' @rdname engine_options
#' @export
`opt_chunk_buffer<-` = function(ctg, value)
{
  assert_is_a_number(value)
  if (value < 0) message("Negative buffers are allowed in lidR but use them cautiously!")
  ctg@chunk_options$buffer <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_chunk_size = function(ctg)
{
  return(ctg@chunk_options$size)
}

#' @rdname engine_options
#' @export
`opt_chunk_size<-` = function(ctg, value)
{
  assert_is_a_number(value)
  assert_all_are_non_negative(value)

  if (value > 0) {
     if (value < 250) {
       message("Be careful, a chunk size smaller than 250 is likely to be irrelevant.")
    }
  }

  ctg@chunk_options$size <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_chunk_alignment = function(ctg)
{
  return(ctg@chunk_options$alignment)
}

#' @rdname engine_options
#' @export
`opt_chunk_alignment<-` = function(ctg, value)
{
  assert_is_numeric(value)
  assert_is_of_length(value, 2)
  ctg@chunk_options$alignment <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
`opt_restart<-` = function(ctg, value)
{
  assert_is_numeric(value)
  assert_is_of_length(value, 1)
  value <- as.integer(value)
  stopifnot(value >= 1)

  if (value == 1)
    ctg@chunk_options$drop <- NULL
  else
    ctg@chunk_options$drop <- 1:(value-1)

  return(ctg)
}

opt_chunk_is_file = function(ctg)
{
  return(ctg@chunk_options$size == 0)
}

# ========= Processing Options ===============

#' @rdname engine_options
#' @export
opt_progress = function(ctg)
{
  return(ctg@processing_options$progress)
}

#' @rdname engine_options
#' @export
`opt_progress<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$progress <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_stop_early = function(ctg)
{
  return(ctg@processing_options$stop_early)
}

#' @rdname engine_options
#' @export
`opt_stop_early<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$stop_early <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_wall_to_wall = function(ctg)
{
  return(ctg@processing_options$wall_to_wall)
}

#' @rdname engine_options
#' @export
`opt_wall_to_wall<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$wall_to_wall <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_independent_files = function(ctg)
{
  return(!opt_wall_to_wall(ctg) & opt_chunk_buffer(ctg) == 0 & opt_chunk_size(ctg) == 0)
}

#' @rdname engine_options
#' @export
`opt_independent_files<-` = function(ctg, value)
{
  assert_is_a_bool(value)

  if (isTRUE(value))
  {
    opt_wall_to_wall(ctg) <- FALSE
    opt_chunk_buffer(ctg) <- 0
    opt_chunk_size(ctg) <- 0
  }
  else
  {
    opt_wall_to_wall(ctg) <- TRUE
    opt_chunk_buffer(ctg) <- 30
    opt_chunk_size(ctg) <- 0
  }

  return(ctg)
}

# =========   Output Options   ===============

#' @rdname engine_options
#' @export
opt_output_files = function(ctg)
{
  return(ctg@output_options$output_files)
}

#' @rdname engine_options
#' @export
`opt_output_files<-` = function(ctg, value)
{
  assert_is_a_string(value)
  ext = tools::file_ext(value)

  if (ext != "")
    warning(glue::glue("{value} contains a file extension. Users don't need to provide a file extension. It will be added automaticaly as a function of the output."))

  value <- gsub("\\{?\\*\\}?", "{ORIGINALFILENAME}", value)
  usefilename <- grepl("\\{ORIGINALFILENAME\\}", value)

  if (usefilename & opt_chunk_size(ctg) > 0)
    message("ORIGINALFILENAME template has been used but the chunk size is not 0. This template makes sense only when processing by file.")

  value <- path.expand(value)
  ctg@output_options$output_files <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_laz_compression = function(ctg)
{
  return(ctg@output_options$drivers$LAS$extension == ".laz")
}

#' @rdname engine_options
#' @export
`opt_laz_compression<-` = function(ctg, value)
{
  assert_is_a_bool(value)

  if (value)
    ctg@output_options$drivers$LAS$extension <- ".laz"
  else
    ctg@output_options$drivers$LAS$extension <- ".las"

  return(ctg)
}

#' @rdname engine_options
#' @export
opt_merge = function(ctg)
{
  if (is.null(ctg@output_options$merge))
    return(TRUE)

  return(ctg@output_options$merge)
}

#' @rdname engine_options
#' @export
`opt_merge<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@output_options$merge <- value
  return(ctg)
}

# =========   Input Options    ===============

#' @rdname engine_options
#' @export
opt_select = function(ctg)
{
  return(ctg@input_options$select)
}

#' @rdname engine_options
#' @export
`opt_select<-` = function(ctg, value)
{
  assert_is_a_string(value)
  ctg@input_options$select <- value
  return(ctg)
}

#' @rdname engine_options
#' @export
opt_filter = function(ctg)
{
  return(ctg@input_options$filter)
}

#' @rdname engine_options
#' @export
`opt_filter<-` = function(ctg, value)
{
  assert_is_a_string(value)
  ctg@input_options$filter <- value
  return(ctg)
}


# ========= Unexported ===============

`opt_copy<-` = function(ctg, value)
{
  assert_is_all_of(value, "LAScatalog")
  ctg@chunk_options <- value@chunk_options
  ctg@processing_options <- value@processing_options
  ctg@output_options <- value@output_options
  ctg@input_options <- value@input_options
  st_crs(ctg) <- st_crs(value)
  return(ctg)
}

