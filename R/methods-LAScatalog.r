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
# ===============================================================================

#' @rdname extent
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAScatalog",
          function(x, ...) {
            return(raster::extent(min(x@data$Min.X), max(x@data$Max.X), min(x@data$Min.Y), max(x@data$Max.Y)))
          }
)

#' @param ... Unused
#' @param drop Unused
#' @rdname redefined_behaviors
#' @export
setMethod("[", "LAScatalog", function(x, i, j, ..., drop = TRUE) {

  ctgname <- deparse(substitute(x))
  iname   <- deparse(substitute(i))
  nargs   <- nargs()

  if (!missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. j must be missing. Maybe you meant: {ctgname}[{iname}, ]."), call. = FALSE)

  if (missing(i) & !missing(j))
    stop(glue::glue("This action is not allowed for a {class(x)}. i cannot be missing."), call. = FALSE)

  if (!missing(i) & missing(j) & nargs == 2L)
    stop(glue::glue("This action is not allowed for a {class(x)}. Maybe you meant: {ctgname}[{iname}, ]."), call. = FALSE)

  y <- callNextMethod()

  new_ctg <- new("LAScatalog")
  new_ctg@chunk_options <- x@chunk_options
  new_ctg@processing_options <- x@processing_options
  new_ctg@output_options     <- x@output_options
  new_ctg@input_options      <- x@input_options
  new_ctg@data               <- y@data
  new_ctg@polygons           <- y@polygons
  new_ctg@plotOrder          <- y@plotOrder
  new_ctg@bbox               <- y@bbox
  new_ctg@proj4string        <- y@proj4string
  return(new_ctg)
})

# #' @rdname redefined_behaviors
# #' @export
# setMethod("[<-", "LAScatalog",  function(x, i, j, value)
# {
#  stop("LAScatalog data are read from standard files and cannot be modified")
# })

#' @rdname redefined_behaviors
#' @export
setMethod("[[<-", "LAScatalog",  function(x, i, j, value)
{
  if (i %in% LASCATALOGATTRIBUTES)
    stop("LAScatalog data read from standard files cannot be modified", call. = FALSE)

  x@data[[i]] = value
  return(x)
})

#' @rdname redefined_behaviors
#' @export
setMethod("$<-", "LAScatalog", function(x, name, value)
{
  if (name %in% LASCATALOGATTRIBUTES)
    stop("LAScatalog data read from standard files cannot be modified", call. = FALSE)

  x@data[[name]] = value
  return(x)
})
