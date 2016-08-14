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



#' Return points with matching conditions
#'
#' Return points with matching conditions. \code{extract} is an overloading
#' function for \code{LAS} objects which replaces the function
#' \code{\link[dplyr:filter]{filter}} from \code{\link[dplyr:dplyr]{dplyr}} package.
#'
#' @aliases extract
#' @param .data An object of class \code{LAS}
#' @param \dots Logical predicates. Multiple conditions are combined with &.
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = lidar %>% extract(Classification == 1, ReturnNumber == 1)
#' @seealso
#' \link[dplyr:filter]{filter}
#' \link[lidR:LAS]{Class LAS}
#' \link[lidR:getFirst]{getFirst}
#' \link[lidR:getFirstLast]{getFirstLast}
#' \link[lidR:getFirstOfMany]{getFirstOfMany}
#' \link[lidR:getSingle]{getSingle}
#' \link[lidR:getLast]{getLast}
#' \link[lidR:getGround]{getGround}
#' \link[lidR:getNth]{getNth}
#' @export extract
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
setGeneric("extract", function(.data, ...){standardGeneric("extract")})

#' @rdname extract
setMethod("extract", "LAS",
	function(.data, ...)
	{
		ret = .data@data %>% dplyr::filter(...) %>% LAS

		return(ret)
	}
)
