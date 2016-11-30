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



#' Retrieve individual pulse
#'
#' Retrieve each individual pulse by attributing to each point a number. The
#' function rely on the GPS time and the return number to retrieve each indivudual
#' beam. Once points are ordered by GPS time the pattern of return number enable
#' to attribute a new number to the points every first return.
#'
#' @param obj A LAS object
#' @return Return nothing. The original object is modified in place by reference.
#'
#' @export detect_pulse
#' @importFrom data.table setorder setNumericRounding uniqueN
setGeneric("detect_pulse", function(obj){standardGeneric("detect_pulse")})

#' @rdname detect_pulse
setMethod("detect_pulse", "LAS",
  function(obj)
  {
    gpstime <- pulseID <- .GRP <- NULL

    fields <- names(obj@data)
    dpulse = NA_real_

    if("gpstime" %in% fields)
    {
      data.table::setNumericRounding(0) # remove rounding for gpstime aggregation
      data.table::setorder(obj@data, gpstime)
      obj@data[, pulseID := .lagisdiff(gpstime)][]
      dpulse <- obj@data$pulseID %>% data.table::uniqueN %>% divide_by(obj@area)
    }
    else
      lidRError("LDR4", infield = "gpstime", outfield = "pulseID", behaviour = warning)

    return(invisible(dpulse))
  }
)


#' Retrieve individual flightlines
#'
#' Retrieve each individual flightline by attributing to each point a number. The
#' function rely on the GPS time to retrieve each indivudual flighlines. In a
#' continuous dataset, once points are ordered by GPS time, the time between two
#' consecutive points cannot be important. If the time between two consecutive
#' points is too long it means that is comes from another flightine. The defaut
#' thresohold is 30 seconds
#'
#' @param obj A LAS object
#' @param dt numeric. The threshold time lag used to retrieve flightlines
#' @return Return nothing. The original object is modified in place by reference.
#'
#' @export detect_flightline
#' @importFrom data.table setorder
setGeneric("detect_flightline", function(obj, dt = 30){standardGeneric("detect_flightline")})

#' @rdname detect_flightline
setMethod("detect_flightline", "LAS",
  function(obj, dt = 30)
  {
    gpstime <- flightlineID <- NULL

    fields <- names(obj@data)

    if("gpstime" %in% fields)
    {
      data.table::setorder(obj@data, gpstime)
      obj@data[, flightlineID := .lagissup(gpstime, dt)][]
    }
    else
      lidRError("LDR4", infield = "gpstime", outfield = "flightlineID", behaviour = warning)

    return(invisible())
  }
)


#' Retrieve individual scanline
#'
#' Retrieve each individual scanline by attributing to each point a number. When
#' data are sampled according to a saw-tooth pattern (oscilating mirror) a scanline
#' is one line, or row of data. The function rely on the GPS field time to
#' order the data. Then, the 'ScanDirectionFlag' field (when avaible) is used to
#' retrieve each scanline
#'
#' @param obj A LAS object
#' @return Return nothing. The original object is modified in place by reference.
#' @export detect_scanline
#' @importFrom data.table setorder
setGeneric("detect_scanline", function(obj){standardGeneric("detect_scanline")})

#' @rdname detect_scanline
setMethod("detect_scanline", "LAS",
  function(obj)
  {
    gpstime <- scanlineID <- ScanDirectionFlag <- NULL

    fields <- names(obj@data)

    if("gpstime" %in% fields)
    {
      data.table::setorder(obj@data, gpstime)

      if ("ScanDirectionFlag" %in% fields)
      {
        values = unique(obj$ScanDirectionFlag)

        if(length(values) == 2 & 1 %in% values & 2 %in% values)
          obj@data[, scanlineID := .lagisdiff(ScanDirectionFlag)][]
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
)

#' @importFrom data.table shift
.lagissup = function(x, dx)
{
  boo = (x - data.table::shift(x)) > dx
  boo[1] = TRUE
  return(cumsum(boo))
}

#' @importFrom data.table shift
.lagisdiff = function(x)
{
  boo = x != data.table::shift(x)
  boo[1] = TRUE
  return(cumsum(boo))
}