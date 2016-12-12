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
#' Select only some returns
#'
#' \itemize{
#' \item{\code{lasfilterfirst} Select only the first returns.}
#' \item{\code{lasfilterfirstlast} Select only the first and last returns.}
#' \item{\code{lasfilterground} Select only the returns classified as ground according to LAS specification v1.3.}
#' \item{\code{lasfilterlast} Select only the last returns i.e. the last returns and the single returns.}
#' \item{\code{lasfilternth} Select the returns from their position in the return sequence.}
#' \item{\code{lasfilterfirstofmany} Select only the first returns from pulses which returned multiple points.}
#' \item{\code{lasfiltersingle} Select only the returns which return only one point.}
#' }
#' @param .las An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @param n the position in the return sequence
#' @return An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' firstReturns  = lidar %>% lasfilterfirst
#' groundReturns = lidar %>% lasfilterground
#' @family lasfilters
#' @name lasfilters
NULL

#' @export lasfilterfirst
#' @family lasfilters
#' @rdname lasfilters
lasfilterfirst = function(.las)
{
  return(lasfilternth(.las, 1))
}

#' @export lasfilterfirstlast
#' @family lasfilters
#' @rdname lasfilters
lasfilterfirstlast = function(.las)
{
  ReturnNumber <- NumberOfReturns <- NULL

  return(lasfilter(.las, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
}

#' @export lasfilterfirstofmany
#' @family lasfilters
#' @rdname lasfilters
lasfilterfirstofmany = function(.las)
{
  NumberOfReturns <- ReturnNumber <- NULL

  return(lasfilter(.las, NumberOfReturns > 1, ReturnNumber == 1))
}

#' @export lasfilterground
#' @family lasfilters
#' @rdname lasfilters
lasfilterground = function(.las)
{
  Classification <- NULL

  return(lasfilter(.las, Classification == 2))
}

#' @family lasfilters
#' @export lasfilterlast
#' @rdname lasfilters
lasfilterlast = function(.las)
{
  NumberOfReturns <- ReturnNumber <- NULL

  return(lasfilter(.las, ReturnNumber == NumberOfReturns))
}

#' @family lasfilters
#' @export lasfilternth
#' @rdname lasfilters
lasfilternth = function(.las, n)
{
  ReturnNumber <- NULL

  return(lasfilter(.las, ReturnNumber == n))
}

#' @family lasfilters
#' @export lasfiltersingle
#' @rdname lasfilters
lasfiltersingle = function(.las)
{
  NumberOfReturns <- NULL

  return(lasfilter(.las, NumberOfReturns == 1))
}

#' @family lasfilters
#' @export lasfilterfirstofmany
#' @rdname lasfilters
lasfilterfirstofmany = function(.las)
{
  NumberOfReturns <- ReturnNumber <- NULL

  return(lasfilter(.las, NumberOfReturns > 1, ReturnNumber == 1))
}
