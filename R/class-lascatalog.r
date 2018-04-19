# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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



#' An S4 class to represent a set of a .las or .laz files
#'
#' A LAScatalog object is a representation of a set of las/laz files
#'
#' @slot data data.table. A table representing the header of each file.
#' @slot crs A \link[sp:CRS]{CRS} object.
#' @seealso
#' \link[lidR:LAS-class]{LAS}
#' \link[lidR:LAS]{LAS}
#' \link[lidR:readLAS]{readLAS}
#' @import data.table
#' @import magrittr
#' @import methods
#' @include class-lasheader.r
#' @importClassesFrom sp CRS
#' @exportClass LAS
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAScatalog",
  representation(
    data = "data.table",
    crs  = "CRS"
  )
)

setMethod("initialize", "LAScatalog", function(.Object, data, crs)
{
  .Object@data <- data
  .Object@crs  <- crs
  return(.Object)
})

#' Build a catalog of las tiles/files
#'
#' Build a \link[lidR:LAScatalog-class]{LAScatalog} object from a folder name. A catalog is the representation of a set
#' of las files. A computer cannot load all the data at the same time. A catalog is a simple
#' way to manage all the file sequentially reading only the headers.
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#' @seealso
#' \link{LAScatalog-class}
#' \link[lidR:plot.LAScatalog]{plot}
#' \link{catalog_apply}
#' \link{catalog_queries}
#' @return A \code{LAScatalog} object
#' @export
catalog <- function(folder, ...)
{
  if (!is.character(folder))
    stop("'folder' must be a character string")

  finfo <- file.info(folder)

  if (all(!finfo$isdir))
    files <- folder
  else if (!dir.exists(folder))
    stop(paste(folder, " does not exist"))
  else
    files <- list.files(folder, full.names = T, pattern = "(?i)\\.la(s|z)$", ...)

  verbose("Reading files...")

  header <- LASheader(rlas::readlasheader(files[1]))
  crs <- epsg2proj(get_epsg(header))

  headers <- lapply(files, function(x)
  {
    header <- rlas::readlasheader(x)
    header$`Variable Length Records` <- NULL
    data.table::setDT(header)
    return(header)
  })

  headers <- data.table::rbindlist(headers)
  headers$filename <- files

  return(new("LAScatalog", headers, crs))
}

setMethod("show", "LAScatalog", function(object)
{
  memsize <- format(utils::object.size(object), units = "auto")
  surface <- area(object)
  npoints <- sum(object@data$`Number of point records`)
  ext     <- extent(object)

  cat("class       : LAScatalog\n")
  cat("memory      :", memsize, "\n")
  cat("extent      :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("area        :", surface, "m\u00B2\n")
  cat("points      :", npoints, "points\n")
  cat("density     :", round(npoints/surface, 1), "points/m\u00B2\n")
  cat("num. files  :", dim(object@data)[1], "\n")
  cat("coord. ref. :", object@crs@projargs, "\n")
})
