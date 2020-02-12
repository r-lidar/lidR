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



#' Filter points of interest with matching conditions
#'
#' Filter points of interest (POI) from  a LAS object where conditions are true.
#'
#' @param las An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @param \dots Logical predicates. Multiple conditions are combined with '&' or ','
#'
#' @return An object of class \code{\link[lidR:LAS-class]{LAS}}
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = filter_poi(lidar, Classification == 2L & ReturnNumber == 1L)
#'
#' # Multiple arguments are equivalent to &
#' firstground = filter_poi(lidar, Classification == 2L, ReturnNumber == 1L)
#'
#' # Multiple criteria
#' first_or_ground = filter_poi(lidar, Classification == 2L | ReturnNumber == 1L)
#' @export
#' @family filters
filter_poi = function(las, ...)
{
  stopifnotlas(las)
  keep <- lasfilter_(las, lazyeval::dots_capture(...))

  # Memory optimization
  if (sum(keep) == nrow(las@data))
    return(las)

  return(LAS(las@data[keep], las@header, las@proj4string, check = FALSE))
}

lasfilter_ <- function(las, conditions)
{
  n <- nrow(las@data)
  combined_bools <- !logical(n)

  for (condition in conditions)
  {
    bools <- lazyeval::f_eval(condition, las@data)

    if (!is.logical(bools))
      stop("`conditions` must be logical.")

    bools[is.na(bools)] <- FALSE
    combined_bools <- combined_bools & bools
  }

  return(combined_bools)
}

#' Predefined point of interest filters
#'
#' Select only some returns
#'
#' \itemize{
#' \item{\code{filter_first} Select only the first returns.}
#' \item{\code{filter_firstlast} Select only the first and last returns.}
#' \item{\code{filter_ground} Select only the returns classified as ground according to LAS specification.}
#' \item{\code{filter_last} Select only the last returns i.e. the last returns and the single returns.}
#' \item{\code{filter_nth} Select the returns from their position in the return sequence.}
#' \item{\code{filter_firstofmany} Select only the first returns from pulses which returned multiple points.}
#' \item{\code{filter_single} Select only the returns that return only one point.}
#' }
#' @param las An object of class \code{\link[lidR:LAS-class]{LAS}}
#' @param n the position in the return sequence
#'
#' @return An object of class \code{\link[lidR:LAS-class]{LAS}}
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' firstReturns  = filter_first(lidar)
#' groundReturns = filter_ground(lidar)
#' @family filters
#' @name filters
NULL

#' @export
#' @family filters
#' @rdname filters
filter_first = function(las)
{
  return(filter_nth(las, 1))
}

#' @export
#' @family filters
#' @rdname filters
filter_firstlast = function(las)
{
  ReturnNumber <- NumberOfReturns <- NULL
  return(filter_poi(las, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
}

#' @export
#' @family filters
#' @rdname filters
filter_firstofmany = function(las)
{
  NumberOfReturns <- ReturnNumber <- NULL
  return(filter_poi(las, NumberOfReturns > 1, ReturnNumber == 1))
}

#' @export
#' @family filters
#' @rdname filters
filter_ground = function(las)
{
  Classification <- NULL
  return(filter_poi(las, Classification == 2))
}

#' @family filters
#' @export
#' @rdname filters
filter_last = function(las)
{
  NumberOfReturns <- ReturnNumber <- NULL
  return(filter_poi(las, ReturnNumber == NumberOfReturns))
}

#' @family filters
#' @export
#' @rdname filters
filter_nth = function(las, n)
{
  ReturnNumber <- NULL
  return(filter_poi(las, ReturnNumber == n))
}

#' @family filters
#' @export
#' @rdname filters
filter_single = function(las)
{
  NumberOfReturns <- NULL
  return(filter_poi(las, NumberOfReturns == 1))
}

