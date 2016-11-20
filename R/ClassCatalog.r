# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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



#' An S4 class to represent a catalog of las tiles.
#'
#' An S4 class to represent a set of las or laz tiles, to plot them and to process them with
#' multicore.
#'
#' A \code{Catalog} object contains a \code{data.frame} in the slot \code{@headers} with the data
#' read from the headers of all user's .las or .laz files. A catalog is the representation
#' of a set of files. A computer cannot load all the data at thet same time. A catalog
#' is a simple way to manage all the files sequentially reading only the headers. See the
#' public documentation of las format specification for more informations.
#' @slot headers data.frame. A table representing the las header data
#' @name Catalog-class
#' @rdname Catalog-class
#' @exportClass Catalog
#' @seealso
#' \link[lidR:Catalog]{Catalog}
#' \link[lidR:plot.Catalog]{plot}
#' \link[lidR:process_parallel]{process_parallel}
#' \link[lidR:roi_query]{roi_query}
setClass(Class = "Catalog", representation = representation(headers = "data.frame"))

setMethod("initialize", "Catalog", function(.Object, folder, ...)
{
  if (!is.character(folder))
    lidRError("GTG1")

  if (!dir.exists(folder))
    lidRError("CTG2")

  files <- list.files(folder, full.names = T, pattern = "(?i)\\.la(s|z)$", ...)

  headers <- lapply(files, function(x)
  {
    readLASheader(x) %>% as.data.frame
  })

  headers <- do.call(rbind.data.frame, headers)
  headers$filename <- files
  rownames(headers) <- NULL

  .Object@headers <- headers

  return(.Object)
})

#' Extract parts of an Catalog object
#'
#' @param x object from which to extract element(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing)
#' @export
setMethod("[", signature(x="Catalog"),
  function(x, i)
  {
    if (missing(i)) { i = 0 }
    y=x
    y@headers=x@headers[i,]
    y
  })
