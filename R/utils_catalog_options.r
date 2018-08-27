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

#' Get or set \link[lidR:LAScatalog-class]{LAScatalog} processing engine options
#'
#' The names of the options and their roles are documented in \link[lidR:LAScatalog-class]{LAScatalog}.
#' The options are used by all the functions that take a \code{LAScatalog} as input.
#'
#' @param ctg An object of class \link[lidR:LAScatalog-class]{LAScatalog}
#' @param value An appropriated value depending on the expected input.
#' @return Functions \code{get_*} return the value of the associated options
#' @rdname catalog_options_tools
#' @export
get_buffer = function(ctg)
{
  return(ctg@clustering_options$buffer)
}

#' @rdname catalog_options_tools
#' @export
`set_buffer<-` = function(ctg, value)
{
  assertive::assert_is_a_number(value)
  if (value < 0) message("Negative buffers are allowed in lidR but you should do that cautiously!")
  ctg@clustering_options$buffer <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_tiling_size = function(ctg)
{
  return(ctg@clustering_options$tiling_size)
}

#' @rdname catalog_options_tools
#' @export
`set_tiling_size<-` = function(ctg, value)
{
  assertive::assert_is_a_number(value)
  assertive::assert_all_are_non_negative(value)
  ctg@clustering_options$tiling_size <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_alignment = function(ctg)
{
  return(ctg@clustering_options$alignment)
}

#' @rdname catalog_options_tools
#' @export
`set_alignment<-` = function(ctg, value)
{
  assertive::assert_is_numeric(value)
  assertive::assert_is_of_length(value, 2)
  ctg@clustering_options$alignment <- value
  return(ctg)
}

get_by_file = function(ctg)
{
  return(ctg@clustering_options$tiling_size == 0)
}


# ========= Processing Options ===============

#' @rdname catalog_options_tools
#' @export
get_cores = function(ctg)
{
  return(ctg@processing_options$cores)
}

#' @rdname catalog_options_tools
#' @export
`set_cores<-` = function(ctg, value)
{
  sys.cores = future::availableCores()
  value = as.integer(value)

  if(value > sys.cores) {
    message(glue::glue("Available cores: {sys.cores}. Number of cores set to {sys.cores}."))
    value = sys.cores
  }

  if(value < 1) {
    message("Number of cores must be positive. Number of cores set to 1.")
    value = 1L
  }

  ctg@processing_options$cores <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_progress = function(ctg)
{
  return(ctg@processing_options$progress)
}

#' @rdname catalog_options_tools
#' @export
`set_progress<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@processing_options$progress <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_stop_early = function(ctg)
{
  return(ctg@processing_options$stop_early)
}

#' @rdname catalog_options_tools
#' @export
`set_stop_early<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@processing_options$stop_early <- value
  return(ctg)
}

# =========   Output Options   ===============

#' @rdname catalog_options_tools
#' @export
get_output_files = function(ctg)
{
  return(ctg@output_options$output_files)
}

#' @rdname catalog_options_tools
#' @export
`set_output_files<-` = function(ctg, value)
{
  assertive::assert_is_a_string(value)
  ext = tools::file_ext(value)

  if (ext != "")
    warning(glue::glue("{value} contains a file extension. User don't need to provide file extension. It will be added automaticaly as a function of the output."), call. = FALSE)

  ctg@output_options$output_files <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_laz_compression = function(ctg)
{
  return(ctg@output_options$drivers$LAS$laz_compression)
}

#' @rdname catalog_options_tools
#' @export
`set_laz_compression<-` = function(ctg, value)
{
  assertive::assert_is_a_bool(value)
  ctg@output_options$drivers$LAS$laz_compression <- value
  return(ctg)
}

# =========   Input Options    ===============

#' @rdname catalog_options_tools
#' @export
get_select = function(ctg)
{
  return(ctg@input_options$select)
}

#' @rdname catalog_options_tools
#' @export
`set_select<-` = function(ctg, value)
{
  assertive::assert_is_a_string(value)
  ctg@input_options$select <- value
  return(ctg)
}

#' @rdname catalog_options_tools
#' @export
get_filter = function(ctg)
{
  return(ctg@input_options$filter)
}

#' @rdname catalog_options_tools
#' @export
`set_filter<-` = function(ctg, value)
{
  assertive::assert_is_a_string(value)
  ctg@input_options$filter <- value
  return(ctg)
}

