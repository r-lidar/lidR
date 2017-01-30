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
#' Write a LAS object into a binary file (.las or .laz specified in filename)
#'
#' @param .las an object of class LAS
#' @param file character. A character string naming an output file
#' @return void
#' @export
writeLAS = function(.las, file)
{
  files <- NULL

  islas = tools::file_ext(file) %in% c("las", "laz")

  if(length(file) > 1)
    lidRError("LAS5", behaviour = stop)

  if(!islas)
    lidRError("LAS2", files = file, behaviour = stop)

  file = path.expand(file)

  I = RN = NoR = SDF = EoF = C = SA = UD = PSI = R = G = B = integer(0)
  time = numeric(0)

  fields = names(.las@data)

  if("Intensity" %in% fields)
    I = .las@data$Intensity
  if("ReturnNumber" %in% fields)
    RN = .las@data$ReturnNumber
  if("NumberOfReturns" %in% fields)
    NoR = .las@data$NumberOfReturns
  if("ScanDirectionFlag" %in% fields)
    SDF = .las@data$ScanDirectionFlag
  if("EdgeOfFlightline" %in% fields)
    EoF = .las@data$EdgeOfFlightline
  if("Classification" %in% fields)
    C = .las@data$Classification
  if("ScanAngle" %in% fields)
    SA = .las@data$ScanAngle
  if("UserData" %in% fields)
    UD = .las@data$UserData
  if("gpstime" %in% fields)
    time = .las@data$gpstime
  if("PointSourceID" %in% fields)
    PSI = .las@data$PointSourceID
  if("R" %in% fields & "G" %in% fields & "B" %in% fields )
  {
    R = .las@data$R
    G = .las@data$G
    B = .las@data$B
  }

  rlas::writelas(file, .las@header@data, .las@data$X, .las@data$Y, .las@data$Z, time, I, RN, NoR, SDF, EoF, C, SA, UD, PSI, R, G, B)
}