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

#' @param data a \link[data.table:data.table]{data.table} containing the data of a las or laz file.
#' @param header a \code{list} or a \link[lidR:LASheader-class]{LASheader} containing the header of
#' a las or laz file.
#' @param proj4string projection string of class \link[sp:CRS-class]{CRS-class}.
#' @param check logical. Conformity tests while building the object.
#' @return An object of class \code{LAS}
#' @export
#' @describeIn LAS-class Create objects of class LAS
LAS <- function(data, header = list(), proj4string = sp::CRS(), check = TRUE)
{
  if (is.data.frame(data))
    data.table::setDT(data)

  if (!data.table::is.data.table(data))
    stop("Invalid parameter data in constructor.")

  rlas::is_defined_coordinates(data, "stop")

  if (is(header, "LASheader"))
    header <- as.list(header)

  if (!is.list(header))
    stop("Wrong header object provided.")

  if (length(header) == 0)
    header <- rlas::header_create(data)

  header <- rlas::header_update(header, data)

  if (check & nrow(data) > 0)
  {
    rlas::check_las_validity(header, data)
    rlas::check_las_compliance(header, data)
  }

  header <- LASheader(header)

  if (is.na(proj4string@projargs))
    proj4string <- projection(header, asText = FALSE)

  las             <- new("LAS")
  las@bbox        <- with(header@PHB, matrix(c(`Min X`, `Min Y`, `Max X`, `Max Y`), ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
  las@header      <- header
  las@data        <- data
  las@proj4string <- proj4string

  return(las)
}


#' Extent
#'
#' Returns an Extent object of a \code{LAS*}.
#'
#' @rdname extent
#' @param x An object of the class \code{LAS} or \code{LAScatalog}
#' @param \dots Unused
#' @return Extent object from \pkg{raster}
#' @seealso \code{\link[raster:extent]{raster::extent}}
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAS",
  function(x, ...) {
    return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
  }
)

#' Inherited but modified methods from sp
#'
#' \code{LAS*} objects are \link[sp:Spatial-class]{Spatial} objects so they inherit several methods
#' from \code{sp}. However, some have modified behaviors to prevent some irrelevant modifications. Indeed,
#' a \code{LAS*} object cannot contain anything, as the content is restricted by the LAS specifications.
#' If a user attempts to use one of these functions inappropriately an informative error will be thrown.
#'
#' @param x A \code{LAS*} object
#' @param name A literal character string or a name (possibly backtick quoted).
#' @param value typically an array-like R object of a similar class as x.
#' @export
#' @rdname redefined_behaviors
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' las$Z = 2L
#' las[["Z"]] = 1:10
#' las$NewCol = 0
#' las[["NewCol"]] = 0
#' }
setMethod("$<-", "LAS", function(x, name, value)
{
  if (!name %in% names(x@data))
    stop("Addition of a new column using $ is forbidden for LAS objects. See ?lasadddata", call. = FALSE)

  if (name %in% LASFIELDS)
  {
    type1 <- storage.mode(x@data[[name]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  x@data[[name]] = value
  return(x)
})

#' @param i string, name of elements to extract or replace.
#' @param j Unused.
#' @rdname redefined_behaviors
#' @export
setMethod("[[<-", c("LAS", "ANY", "missing", "ANY"),  function(x, i, j, value)
{
  if (!i %in% names(x@data))
    stop("Addition of a new column using [[ is forbidden for LAS objects. See ?lasadddata", call. = FALSE)

  if (i %in% LASFIELDS)
  {
    type1 <- storage.mode(x@data[[i]])
    type2 <- storage.mode(value)

    if (type1 != type2)
      stop(glue::glue("Trying to replace data of type {type1} by data of type {type2}: this action is not allowed"), call. = FALSE)
  }

  x@data[[i]] = value
  return(x)
})
