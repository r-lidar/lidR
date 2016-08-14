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
#' The informations keep in the header are those read only from the first file of the list.
#' The optional logical parameters enable to do not load some information from the files to save memory if these informations
#' are useless for user's purpose. Indeed, the readLAS function does no stream the data. Everything is loaded into the
#' computer memory (RAM) in a not completely optimized way (R does not enable to manage many data type).
#'
#' @param files array of character.
#' @param Intensity logical. do you want to load Intensity field? default: TRUE
#' @param ReturnNumber logical. do you want to load ReturnNumber field? default: TRUE
#' @param NumberOfReturns logical. do you want to load NumberOfReturns field? default: TRUE
#' @param ScanDirectionFlag logical. do you want to load ScanDirectionFlag field? default: FALSE
#' @param EdgeofFlightline logical. do you want to load EdgeofFlightline field? default: FALSE
#' @param Classification logical. do you want to load Classification field? default: TRUE
#' @param ScanAngle logical. do you want to load intensity field? default: TRUE
#' @param UserData logical. do you want to load UserData field? default: FALSE
#' @param PointSourceID logical. do you want to load PointSourceID field? default: FALSE
#' @param RGB logical. do you want to load intensity R,G and B? default: TRUE
#'
#' @return A LAS object
#' @export readLAS
#' @seealso
#' \link[lidR:LAS-class]{Class LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#' @importFrom data.table data.table rbindlist
readLAS = function(files,
                   Intensity = TRUE,
                   ReturnNumber = TRUE,
                   NumberOfReturns = TRUE,
                   ScanDirectionFlag = FALSE,
                   EdgeofFlightline = FALSE,
                   Classification = TRUE,
                   ScanAngle = TRUE,
                   UserData = FALSE,
                   PointSourceID = FALSE,
                   RGB = TRUE)
{
  valid = file.exists(files)
  islas = tools::file_ext(files) %in% c("las", "laz")

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

  data = lapply(files, function(x)
  {
    as.data.table(readLASdata(x, Intensity,
                              ReturnNumber,
                              NumberOfReturns,
                              ScanDirectionFlag,
                              EdgeofFlightline,
                              Classification,
                              ScanAngle,
                              UserData,
                              PointSourceID,
                              RGB))
  })

  data = data.table::rbindlist(data)

  if(dim(data)[1] == 0 & dim(data)[2] == 0)
    return(invisible(NULL))

  las = LAS(data)
  las@header = readLASheader(files[1])

  return(las)
}
