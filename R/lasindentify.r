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
  gpstime <- pulseID <- .GRP <- NULL

  stopifnotlas(.las)

  fields <- names(.las@data)
  dpulse = NA_real_

  if("gpstime" %in% fields)
  {
    data.table::setorder(.las@data, gpstime)
    .las@data[, pulseID := .lagisdiff(gpstime)][]
  }
  else
    lidRError("LDR4", infield = "gpstime", outfield = "pulseID", behaviour = warning)

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
  gpstime <- flightlineID <- NULL

  stopifnotlas(.las)

  fields <- names(.las@data)

  if("gpstime" %in% fields)
  {
    data.table::setorder(.las@data, gpstime)
    .las@data[, flightlineID := .lagissup(gpstime, dt)][]
  }
  else
    lidRError("LDR4", infield = "gpstime", outfield = "flightlineID", behaviour = warning)

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
  gpstime <- scanlineID <- ScanDirectionFlag <- NULL

  stopifnotlas(.las)

  fields <- names(.las@data)

  if("gpstime" %in% fields)
  {
    data.table::setorder(.las@data, gpstime)

    if ("ScanDirectionFlag" %in% fields)
    {
      values = unique(.las$ScanDirectionFlag)

      if(length(values) == 2 & 1 %in% values & 2 %in% values)
        .las@data[, scanlineID := .lagisdiff(ScanDirectionFlag)][]
      else
        lidRError("LDR8", behaviour = warning)
    }
    else
      lidRError("LDR4", infield = "ScanDirectionFlag", outfield = "scanlineID", behaviour = warning)
  }
  else
    lidRError("LDR4", infield = "gpstime", outfield = "scanlineID", behaviour = warning)

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
