# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel
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
# ==============================================================================


# ========= Clustering Options ===============

#' Get or set LAScatalog processing engine options
#'
#' The names of the options and their roles are documented in \link[lidR:LAScatalog-class]{LAScatalog}.
#' The options are used by all the functions that support a \code{LAScatalog} as input.
#'
#' @param ctg An object of class \link[lidR:LAScatalog-class]{LAScatalog}
#' @param value An appropriate value depending on the expected input.
#'
#' @name catalog_options_tools
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ctg = catalog(LASfile)
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
NULL

#' @rdname catalog_options_tools
#' @export
opt_chunk_buffer = function(ctg)
{
  return(ctg@chunk_options$buffer)
}

#' @rdname catalog_options_tools
#' @export
`opt_chunk_buffer<-` = function(ctg, value)
{
  assert_is_a_number(value)
  if (value < 0) message("Negative buffers are allowed in lidR but use them cautiously!")
  ctg@chunk_options$buffer <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_chunk_size = function(ctg)
{
  return(ctg@chunk_options$size)
}

#' @rdname catalog_options_tools
#' @export
`opt_chunk_size<-` = function(ctg, value)
{
  assert_is_a_number(value)
  assert_all_are_non_negative(value)
  ctg@chunk_options$size <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_chunk_alignment = function(ctg)
{
  return(ctg@chunk_options$alignment)
}

#' @rdname catalog_options_tools
#' @export
`opt_chunk_alignment<-` = function(ctg, value)
{
  assert_is_numeric(value)
  assert_is_of_length(value, 2)
  ctg@chunk_options$alignment <- value
  return(ctg)
}

opt_chunk_is_file = function(ctg)
{
  return(ctg@chunk_options$size == 0)
}


# ========= Processing Options ===============

#' @rdname catalog_options_tools
#' @export
opt_cores = function(ctg)
{
  return(ctg@processing_options$cores)
}

#' @rdname catalog_options_tools
#' @export
`opt_cores<-` = function(ctg, value)
{
  sys.cores = future::availableCores()
  value = as.integer(value)

  if (value > sys.cores) {
    message(glue::glue("Available cores: {sys.cores}. Number of cores set to {sys.cores}."))
    value = sys.cores
  }

  if (value < 1) {
    message("Number of cores must be positive. Number of cores set to 1.")
    value = 1L
  }

  ctg@processing_options$cores <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_progress = function(ctg)
{
  return(ctg@processing_options$progress)
}

#' @rdname catalog_options_tools
#' @export
`opt_progress<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$progress <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_stop_early = function(ctg)
{
  return(ctg@processing_options$stop_early)
}

#' @rdname catalog_options_tools
#' @export
`opt_stop_early<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$stop_early <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_wall_to_wall = function(ctg)
{
  return(ctg@processing_options$wall_to_wall)
}

#' @rdname catalog_options_tools
#' @export
`opt_wall_to_wall<-` = function(ctg, value)
{
  assert_is_a_bool(value)
  ctg@processing_options$wall_to_wall <- value
  return(ctg)
}


# =========   Output Options   ===============

#' @rdname catalog_options_tools
#' @export
opt_output_files = function(ctg)
{
  return(ctg@output_options$output_files)
}

#' @rdname catalog_options_tools
#' @export
`opt_output_files<-` = function(ctg, value)
{
  assert_is_a_string(value)
  ext = tools::file_ext(value)

  if (ext != "")
    warning(glue::glue("{value} contains a file extension. Users don't need to provide a file extension. It will be added automaticaly as a function of the output."), call. = FALSE)

  ctg@output_options$output_files <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_laz_compression = function(ctg)
{
  return(ctg@output_options$drivers$LAS$extension == ".laz")
}

#' @rdname catalog_options_tools
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

# =========   Input Options    ===============

#' @rdname catalog_options_tools
#' @export
opt_select = function(ctg)
{
  return(ctg@input_options$select)
}

#' @rdname catalog_options_tools
#' @export
`opt_select<-` = function(ctg, value)
{
  assert_is_a_string(value)
  ctg@input_options$select <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
opt_filter = function(ctg)
{
  return(ctg@input_options$filter)
}

#' @rdname catalog_options_tools
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
  ctg@proj4string <- value@proj4string
  return(ctg)
}

