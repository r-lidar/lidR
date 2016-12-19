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



#' Read las or laz files
#'
#' Read las or laz files in format 1 to 4 according to LAS specification and return an object of class LAS
#'
#' If several files are given the returned LAS object is considered as one LAS file.
#' The information retained in the header will be read from the first file in the list.
#' The optional logical parameters enable the user to save memory by choosing to load only the fields they need. Indeed,
#' the readLAS function does not 'stream' the data. Data is loaded into the
#' computer's memory (RAM) suboptimally because R does not accommodate many different data types.
#'
#' @param files array of characters or a \link[lidR:catalog]{Catalog} object
#' @param Intensity logical. do you want to load the Intensity field? default: TRUE
#' @param ReturnNumber logical. do you want to load the ReturnNumber field? default: TRUE
#' @param NumberOfReturns logical. do you want to load the NumberOfReturns field? default: TRUE
#' @param ScanDirectionFlag logical. do you want to load the ScanDirectionFlag field? default: FALSE
#' @param EdgeOfFlightline logical. do you want to load the EdgeOfFlightline field? default: FALSE
#' @param Classification logical. do you want to load the Classification field? default: TRUE
#' @param ScanAngle logical. do you want to load the ScanAngle field? default: TRUE
#' @param UserData logical. do you want to load the UserData field? default: FALSE
#' @param PointSourceID logical. do you want to load the PointSourceID field? default: FALSE
#' @param RGB logical. do you want to load R,G and B fields? default: TRUE
#' @param pulseID logical. do you want to compute the extra field \link[lidR:laspulse]{pulseID}? default: TRUE
#' @param flightlineID logical. do you want to compute the extra field \link[lidR:lasflightline]{flightlineID}? default: FALSE
#' @param color logical. do you want to compute the extra field \link[lidR:lascolor]{color}? default: FALSE
#' @param XYZonly logical. Overwrite all other options. Load only X, Y, Z fields. default: FALSE
#' @param all logical. Overwrite all other options. Load everything. default: FALSE
#'
#' @return A LAS object
#' @export readLAS
#' @seealso
#' \link[lidR:LAS-class]{Class LAS}
#' \link[lidR:catalog]{Catalog}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
readLAS = function(files,
                   Intensity = TRUE,
                   ReturnNumber = TRUE,
                   NumberOfReturns = TRUE,
                   ScanDirectionFlag = FALSE,
                   EdgeOfFlightline = FALSE,
                   Classification = TRUE,
                   ScanAngle = TRUE,
                   UserData = FALSE,
                   PointSourceID = FALSE,
                   RGB = TRUE,
                   pulseID = TRUE,
                   flightlineID = FALSE,
                   color = FALSE,
                   XYZonly = FALSE,
                   all = FALSE)
{
  if(class(files)[1] == "Catalog")
    files = files$filename

  valid = file.exists(files)
  islas = tools::file_ext(files) %in% c("las", "laz", "LAS", "LAZ")

  if( sum(!valid) > 0)
  {
    lidRError("LAS1", files = files[!valid], behaviour = warning)
    files = files[valid]
  }

  if( sum(!islas) > 0 )
  {
    lidRError("LAS2", files = files[!islas], behaviour = warning)
    files = files[islas]
  }

  if (sum(valid) == 0 | sum(islas) == 0)
    lidRError("LAS3", behaviour = stop)

  files = normalizePath(files)

  if(XYZonly)
  {
    Intensity <- ReturnNumber <- NumberOfReturns <- ScanDirectionFlag <- FALSE
    EdgeOfFlightline <- Classification <- ScanAngle <- UserData <- FALSE
    PointSourceID <- RGB <- pulseID <- flightlineID <- FALSE
  }

  if(all)
  {
    Intensity <- ReturnNumber <- NumberOfReturns <- ScanDirectionFlag <- TRUE
    EdgeOfFlightline <- Classification <- ScanAngle <- UserData <- TRUE
    PointSourceID <- RGB <- pulseID <- flightlineID <- TRUE
  }


  data = lapply(files, function(file)
  {
    rlas::readlasdata(file, Intensity, ReturnNumber, NumberOfReturns,
                      ScanDirectionFlag, EdgeOfFlightline, Classification,
                      ScanAngle, UserData, PointSourceID, RGB)
  })

  data = data.table::rbindlist(data)

  if(nrow(data) == 0 | ncol(data) == 0)
    return(invisible())

  header = rlas::readlasheader(files[1])

  las = LAS(data, header)

  if(pulseID)
    laspulse(las)

  if(flightlineID)
    lasflightline(las, 30)

  if(color)
    lascolor(las)

  gc()

  return(las)
}
