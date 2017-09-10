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

#' Extent
#'
#' Returns an Extent object of a \code{LAS} or \code{LAScatalog} object.
#'
#' @rdname extent
#' @param x An object of the class \code{LAS} or \code{LAScatalog}
#' @param \dots Unused
#' @return Extent object from \pkg{raster}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' extent(las)
#' @seealso \code{\link[raster:extent]{raster::extent} }
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAS",
	function(x, ...)
	{
		return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
	}
)

#' @rdname extent
#' @export
#' @importMethodsFrom raster extent
setMethod("extent", "LAScatalog",
  function(x, ...)
  {
    return(raster::extent(min(x@data$`Min X`), max(x@data$`Max X`), min(x@data$`Min Y`), max(x@data$`Max Y`)))
  }
)
