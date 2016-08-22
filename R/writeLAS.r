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



#' Write a las or laz file
#'
#' Write a LAS object into a binary file (.las or .laz m specified in filename)
#'
#' @param obj an object of class LAS
#' @param file character. A character string naming an output file
#' @return void
#' @export writeLAS
setGeneric("writeLAS", function(obj, file){standardGeneric("writeLAS")})

#' @rdname writeLAS
setMethod("writeLAS", "LAS",
  function(obj, file)
  {
    files <- NULL

    islas = tools::file_ext(file) %in% c("las", "laz")

    if(length(file) > 1)
      lidRError("LAS5", behaviour = stop)

    if(!islas)
      lidRError("LAS2", files = file, behaviour = stop)

    file = normalizePath(file)

    I = RN = NoR = SDF = EoF = C = SA = PSI = R = G = B = integer(0)
    time = numeric(0)

    fields = names(obj@data)

    if("Intensity" %in% fields)
      I = obj@data$Intensity
    if("ReturnNumber" %in% fields)
      RN = obj@data$ReturnNumber
    if("NumberOfReturns" %in% fields)
      NoR = obj@data$NumberOfReturns
    if("ScanDirectionFlag" %in% fields)
      SDF = obj@data$ScanDirectionFlag
    if("EdgeofFlightline" %in% fields)
      EoF = obj@data$EdgeofFlightline
    if("Classification" %in% fields)
      C = obj@data$Classification
    if("ScanAngle" %in% fields)
      SA = obj@data$ScanAngle
    if("UserData" %in% fields)
      US = obj@data$UserData
    if("gpstime" %in% fields)
      time = obj@data$gpstime
    if("PointSourceID" %in% fields)
      PSI = obj@data$PointSourceID
    if("R" %in% fields & "G" %in% fields & "B" %in% fields )
    {
      R = obj@data$R
      G = obj@data$G
      B = obj@data$B
    }

    LASlibWrite(file, obj@header, obj@data$X, obj@data$Y, obj@data$Z, I, RN, NoR, SDF, EoF, C, SA, PSI, time, R, G, B)
  }
)