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



#' Transform a raster into a DTM
#'
#' Transform a raster (formal class RasterLayer from package raster) into a
#' DTM format used by the package lidR.
#'
#' @param r a RasterLayer from package raster
#' @seealso
#' \link[raster:raster]{raster}
#' @export
#' @importFrom raster extent res as.matrix
as.DTM = function(r)
{
  if(class(r)[1] != "RasterLayer")
    stop("Not a  RasterLayer")

  resol = raster::res(r)[1]

  z = raster::as.matrix(r)
  z = t(z[nrow(z):1,,drop=FALSE])

  nrow = dim(r)[1]
  ncol = dim(r)[2]
  ext = raster::extent(r)

  x = seq(ext@xmin, ext@xmax-resol, by = resol)
  y = seq(ext@ymin, ext@ymax-resol, by = resol)

  dtm = list(x = x, y = y, z = z)
  attr(dtm, "res") = resol
  class(dtm) = c("DTM", "list")

  return(dtm)
}