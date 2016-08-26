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



#' Filter returns by their position in the return sequence.
#'
#' Select the returns from their position in the return sequence. Point density
#' pulse density and area are recomputed on the fly
#'
#' @aliases  getNth
#' @param obj An object of class \code{LAS}
#' @param n numeric. The position in the return sequence
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' secondReturns = lidar %>% getNth(2)
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:lasfilter]{lasfilter} }
#' @export getNth
#' @note \code{getNth(obj, n)} is an alias for \code{lasfilter(obj, ReturnNumber == n)}
#' @aliases  getNth
setGeneric("getNth", function(obj, n){standardGeneric("getNth")})

#' @rdname getNth
setMethod("getNth", "LAS",
	function(obj, n)
	{
	   ReturnNumber <- NULL

	  if(n > max(obj@data$ReturnNumber) | n <= 0)
	    lidRError("LDR5")

		return(lasfilter(obj, ReturnNumber == n))
	}
)
