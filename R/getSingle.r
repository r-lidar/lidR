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



#' Filter single returns
#'
#' Select only the returns which return only one point.
#'
#' @aliases  getSingle
#' @param obj An object of class \code{LAS}
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' singleReturns = lidar %>% getSingle
#' @family getters
#' @seealso
#' \link[lidR:lasfilter]{lasfilter}
#' @export getSingle
#' @note \code{getSingle(obj)} is an alias for \code{lasfilter(obj, NumberOfReturns == 1))}
setGeneric("getSingle", function(obj){standardGeneric("getSingle")})

#' @rdname getSingle
setMethod("getSingle", "LAS",
	function(obj)
	{
	  NumberOfReturns <- NULL

		return(lasfilter(obj, NumberOfReturns == 1))
	}
)
