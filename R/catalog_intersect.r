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

#' Subset a LAScatalog with a Spatial* object
#'
#' Subset a LAScatalog with a Spatial* object to keep only the tiles of interest. Internally, it uses the
#' function \link[raster:intersect]{intersect} from \code{raster} with a tweak to make it work
#' with a LAScatalog. It can be used to select tiles of interest that encompass Spatial* objects such
#' as SpatialPoints, SpatialPolygons or SpatialLines.
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object
#'
#' @param y Extent, Raster*, SpatialPolygons*, SpatialLines* or SpatialPoints* object
#'
#' @return A LAScatalog
#'
#' @export
catalog_intersect = function(ctg, y)
{
  assert_is_all_of(ctg, "LAScatalog")
  spdf <- as.spatial(ctg)
  spdf$PolygonID <- 1:nrow(spdf@data)
  i <- raster::intersect(spdf, y)$PolygonID
  return(ctg[i,])
}
