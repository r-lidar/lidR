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


#' Surface covered by a \code{LAS} object or by a \code{LAScatalog}.
#'
#' The area is computed with the convex hull for \code{LAS} objects or x,y coordinates. If
#' the data is not convex, the resulting area is only an approximation. The area is computed
#' as the sum of the extents of each file for a \code{LAScatalog}.
#'
#' @param x An object of the class \code{LAS} or \code{LAScatalog} or numeric
#' @param y If x is numeric, then provide also y.
#' @param ... unused
#' @return numeric. The area of the object computed in the same units as the coordinate reference system
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' area(las)
#' @seealso \code{\link[raster:area]{raster::area} }
#' @export
#' @importMethodsFrom raster area
setGeneric("area", function(x, ...) standardGeneric("area"))

#' @rdname area
#' @export
#' @importMethodsFrom raster area
setMethod("area", "LAS",
  function(x, ...)
  {
    return(area(x@data$X, x@data$Y))
  }
)

#' @rdname area
#' @export
#' @importMethodsFrom raster area
setMethod("area", "LAScatalog",
  function(x, ...)
  {
    x <- x@data
    area <- sum((x$`Max X` - x$`Min X`) * (x$`Max Y` - x$`Min Y`))
    return(area)
  }
)

#' @rdname area
#' @export
#' @importMethodsFrom raster area
setMethod("area", "numeric",
  function(x, y, ...)
  {
    stopifnot(length(x) == length(y))
    hull <- convex_hull(x, y)
    area <- polygon_area(hull$x, hull$y)
    return(area)
  }
)
