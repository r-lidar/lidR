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


#' Filter duplicated points
#'
#' Filter points that appear more than once in the point cloud according to their X Y Z coordinates
#'
#' @template param-las
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasfilter
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @family filters
filter_duplicates = function(las)
{
  UseMethod("filter_duplicates", las)
}

#' @export
filter_duplicates.LAS = function(las)
{
  dup_xyz <- duplicated(las@data, by = c("X", "Y", "Z"))
  return(filter_poi(las, dup_xyz == FALSE))
}

#' @export
filter_duplicates.LAScluster = function(las)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- filter_duplicates(x)
  x <- filter_poi(x, buffer == 0)
  return(x)
}

#' @export
filter_duplicates.LAScatalog = function(las)
{
  opt_select(las) <- "*"

  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, filter_duplicates, .options = options)
  return(output)
}
