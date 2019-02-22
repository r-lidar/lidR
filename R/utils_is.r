# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2019 Jean-Romain Roussel
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

#' Test if a \code{LAS} object is empty
#'
#' An empty \code{LAS} object is a point cloud with 0 points
#'
#' @param object A \code{LAS} object
#' @param ... Unused
#'
#' @return TRUE or FALSE
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#' is.empty(las)
#'
#' las = new("LAS")
#' is.empty(las)
#' @export
setGeneric("is.empty", function(object, ...)
  standardGeneric("is.empty"))

#' @export
#' @rdname is.empty
setMethod("is.empty", "LAS", function(object, ...)
{
  empty = if (nrow(object@data) == 0) TRUE else FALSE
  return(empty)
})


is.overlapping = function(catalog)
{
  contour       <- rgeos::gUnaryUnion(catalog)
  actual_area   <- round(contour@polygons[[1]]@area, 4)
  measured_area <- round(area(catalog), 4)
  return(actual_area < measured_area)
}


is.indexed = function(catalog)
{
  laxfiles <- paste0(tools::file_path_sans_ext(catalog@data$filename), ".lax")
  return(!any(!file.exists(laxfiles)))
}
