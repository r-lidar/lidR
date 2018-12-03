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

#' Transform a LAS* object into an sp object
#'
#' LAS and LAScatalog objects are transformed into SpatialPointsDataFrame and SpatialPolygonsDataFrame objects, respectively.
#'
#' @param x an object from the lidR package
#' @return An object from sp
#' @export
#' @family cast
as.spatial = function(x)
{
  UseMethod("as.spatial", x)
}

#' @export
as.spatial.LAS = function(x)
{
  sp::SpatialPointsDataFrame(x@data[, 1:2], x@data[, 3:ncol(x@data)], proj4string = x@proj4string)
}

#' @export
as.spatial.LAScatalog = function(x)
{
  res <- new("SpatialPolygonsDataFrame")
  res@bbox <- x@bbox
  res@proj4string <- x@proj4string
  res@plotOrder <- x@plotOrder
  res@data <- x@data
  res@polygons <- x@polygons
  return(res)
}
