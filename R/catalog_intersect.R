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

#' Subset a LAScatalog with a spatial object
#'
#' Subset a LAScatalog with a spatial object to keep only the tiles of interest. It can be
#' used to select tiles of interest that encompass spatial objects such as Spatial* objects,
#' Raster* objects or sf and sfc objects
#'
#' @param ctg A \link[=LAScatalog-class]{LAScatalog} object
#'
#' @param y Extent, Raster*, Spatial*, sf, sfc, Extent or bbox objects
#'
#' @return A LAScatalog
#'
#' @export
catalog_intersect = function(ctg, y)
{
  assert_is_all_of(ctg, "LAScatalog")

  i <- NULL

  if (is(y, "Extent") | is(y, "Raster"))
  {
    crs <- if (is(y, "Raster")) sf::st_crs(y) else sf::st_crs(ctg)
    y <- sf::st_as_sfc(sf::st_bbox(y))
    sf::st_crs(y) <- crs
  }

  if (is(y, "Spatial"))
    y <- sf::st_as_sf(y)

  if (is(y, "sf"))
    y <- sf::st_geometry(y)

  if (is(y, "sfc"))
  {
    sfctg <- sf::st_as_sf(ctg)
    i <- if (is(y, "sfc_POINT")) sf::st_within(y, sfctg, sparse = T) else sf::st_intersects(y, sfctg, sparse = T)
    i <- Filter(function(x) {length(x) > 0}, i)
    i <- Reduce(c, i)
    i <- unique(i)
  }

  if (is.null(i))
    stop("Not supported input geometry", call. = FALSE)

  return(ctg[i,])
}
