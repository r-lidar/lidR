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



#' Extent
#'
#' Returns an Extent object of a \code{LAS} object.
#'
#' @aliases extent
#' @param x An object of the class \code{LAS}
#' @param \dots Unused
#' @return Extent object
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' extent(lidar)
#' @seealso \code{\link[raster:extent]{raster::extent} }
#' @export extent
#' @importMethodsFrom raster extent
setMethod("extent", "LAS",
	function(x)
	{
	  stopifnotlas(x)
		return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
	}
)

#' @export
#' @rdname extent-LAS-method
lasextent <- extent
