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

#' Predefined filters
#'
#' Select only the some returns
#'
#' \itemize{
#' \item{\code{getFirst} Select only the first returns.}
#' \item{\code{getFistLast} Select only the first and last returns.}
#' \item{\code{getGround} Select only the returns classified as ground according to LAS specification v1.3.}
#' \item{\code{getLast} Select only the last returns i.e. the last returns and the single returns.}
#' \item{\code{getNth} Select the returns from their position in the return sequence.}
#' \item{\code{gerFirstOfMany} Select only the first returns from pulses which returned multiple points.}
#' \item{\code{getSingle} Select only the returns which return only one point.}
#' }
#' @param .las An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @return An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' firstReturns  = lidar %>% getFirst
#' groundReturns = lidar %>% getGround
#' @family lasfilters
#' @name getters
NULL

#' @export getFirst
#' @family lasfilters
#' @rdname getters
setGeneric("getFirst", function(.las){standardGeneric("getFirst")})

#' @rdname getters
setMethod("getFirst", "LAS",
	function(.las)
	{
		return(getNth(.las, 1))
	}
)

#' @export getFirstLast
#' @family lasfilters
#' @rdname getters
setGeneric("getFirstLast", function(.las){standardGeneric("getFirstLast")})

#' @rdname getters
setMethod("getFirstLast", "LAS",
	function(.las)
	{
	  ReturnNumber <- NumberOfReturns <- NULL

		return(lasfilter(.las, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
	}
)

#' @export getFirstOfMany
#' @family lasfilters
#' @rdname getters
setGeneric("getFirstOfMany", function(.las){standardGeneric("getFirstOfMany")})

#' @rdname getters
setMethod("getFirstOfMany", "LAS",
	function(.las)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(lasfilter(.las, NumberOfReturns > 1, ReturnNumber == 1))
	}
)

#' @family lasfilters
#' @export getLast
#' @rdname getters
setGeneric("getLast", function(.las){standardGeneric("getLast")})

#' @rdname  getters
setMethod("getLast", "LAS",
	function(.las)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(lasfilter(.las, ReturnNumber == NumberOfReturns))
	}
)

#' @family lasfilters
#' @export getNth
#' @rdname getters
setGeneric("getNth", function(.las, n){standardGeneric("getNth")})

#' @rdname getters
setMethod("getNth", "LAS",
	function(.las, n)
	{
	  ReturnNumber <- NULL
		return(lasfilter(.las, ReturnNumber == n))
	}
)

#' @family lasfilters
#' @export getSingle
#' @rdname getters
setGeneric("getSingle", function(.las){standardGeneric("getSingle")})

#' @rdname getters
setMethod("getSingle", "LAS",
	function(.las)
	{
	  NumberOfReturns <- NULL

		return(lasfilter(.las, NumberOfReturns == 1))
	}
)

#' @family lasfilters
#' @export getFirstOfMany
#' @rdname getters
setGeneric("getFirstOfMany", function(.las){standardGeneric("getFirstOfMany")})

#' @rdname getters
setMethod("getFirstOfMany", "LAS",
	function(.las)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(lasfilter(.las, NumberOfReturns > 1, ReturnNumber == 1))
	}
)
