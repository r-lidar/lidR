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



#' Retrieve individual pulses
#'
#' Retrieve each individual pulse by attributing a number to each point. The
#' function depends on the GPS time to retrieve each individual
#' beam. A column '\code{pulseID}' is added in the slot \code{@data}
#'
#' @param .las A LAS object
#' @return Return nothing. The original object is modified in place by reference.
#'
#' @export laspulse
laspulse = function(.las)
{
  stopifnotlas(.las)

  if(!"gpstime" %in% names(.las@data))
  {
    stop("No gpstime field found. pulseID cannot be computed from this file.")
    return(invisible())
  }

  gpstime <- pulseID <- NULL
  data.table::setorder(.las@data, gpstime)
  .las@data[, pulseID := .lagisdiff(gpstime)][]
  return(invisible())
}


#' Retrieve individual flightlines
#'
#' Retrieve each individual flightline by attributing a number to each point. The
#' function depends on the GPS time to retrieve each individual flightline. In a
#' continuous dataset, once points are ordered by GPS time, the time between two
#' consecutive points does not exceed a few milliseconds. If the time between two consecutive
#' points is too long it means that the second point is from a different flightline. The default
#' threshold is 30 seconds. A column  '\code{flightlineID}' is added in the slot  \code{@data}
#'
#' @param .las A LAS object
#' @param dt numeric. The threshold time lag used to retrieve flightlines
#' @return Return nothing. The original object is modified in place by reference.
#'
#' @export lasflightline
lasflightline = function(.las, dt = 30)
{
  stopifnotlas(.las)
  assertive::assert_is_a_number(dt)
  assertive::assert_all_are_non_negative(dt)

  if(!"gpstime" %in% names(.las@data))
  {
    warning("No gpstime field found. flightlineID cannot be computed from this file.", call. = FALSE)
    return(invisible())
  }

  gpstime <- flightlineID <- NULL
  data.table::setorder(.las@data, gpstime)
  .las@data[, flightlineID := .lagissup(gpstime, dt)][]
  return(invisible())
}


#' Retrieve individual scanline
#'
#' Retrieve each individual scanline by attributing to each point a number. When
#' data are sampled according to a saw-tooth pattern (oscillating mirror) a scanline
#' is one line, or row of data. The function relies on the GPS field time to
#' order the data. Then, the 'ScanDirectionFlag' field (when available) is used to
#' retrieve each scanline  A column '\code{scanline}' is added in the slot \code{@data}
#'
#' @param .las A LAS object
#' @return Return nothing. The original object is modified in place by reference.
#' @export lasscanline
lasscanline = function(.las)
{
  stopifnotlas(.las)

  if(!"gpstime" %in% names(.las@data))
  {
    warning("No gpstime field found. scanlineID cannot be computed from this file.", call. = FALSE)
    return(invisible())
  }

  if (!"ScanDirectionFlag" %in% names(.las@data))
  {
    warning("No gpstime field found. scanlineID cannot be computed from this file.", call. = FALSE)
    return(invisible())
  }

  gpstime <- scanlineID <- ScanDirectionFlag <- NULL
  data.table::setorder(.las@data, gpstime)
  values <- unique(.las@data$ScanDirectionFlag)

  if(!all(sort(values) == c(0L,1L)))
  {
    warning("ScanDirectionFlag field is not properly populated according to LAS specifications. Cannot compute 'scanlineID'", call. = FALSE)
    return(invisible())
  }

  .las@data[, scanlineID := .lagisdiff(ScanDirectionFlag)][]
  return(invisible())
}

.lagissup = function(x, dx)
{
  boo = (x - data.table::shift(x)) > dx
  boo[1] = TRUE
  return(cumsum(boo))
}

.lagisdiff = function(x)
{
  boo = x != data.table::shift(x)
  boo[1] = TRUE
  return(cumsum(boo))
}
