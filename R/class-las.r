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



#' An S4 class to represent the data read in a .las or .laz file
#'
#' A LAS object is the representation of a las/laz files.
#'
#' A \code{LAS} object contains a
#' \code{data.table} in the slot \code{@data} with the data read from a \code{las/laz} file,
#' a \link[lidR:LASheader-class]{LASheader} in the slot \code{@header} (see the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#' for more information) and a \link[sp:CRS]{CRS} (coordinates reference system) in the
#' slot \code{@crs}. In the slot \code{@data} The fields read from the las/laz file are named:
#' \itemize{
#' \item{\code{X Y Z}}
#' \item{\code{Intensity}}
#' \item{\code{ReturnNumber}}
#' \item{\code{NumberOfReturns}}
#' \item{\code{ScanDirectionFlag}}
#' \item{\code{EdgeOfFlightline}}
#' \item{\code{Classification}}
#' \item{\code{ScanAngle}}
#' \item{\code{UserData}}
#' \item{\code{PointSourceID}}
#' }
#' 3 other fields can be computed is desired:
#' slot \code{@data}:
#' \itemize{
#' \item{\code{pulseID}: a unique identifying number for each pulse so the
#' beam origin of each point is known (see \link[lidR:laspulse]{laspulse})}
#'
#' \item{\code{flightlineID}: a unique identifying number for the flightline
#' so the flightline origin of each point is known (see \link[lidR:lasflightline]{lasflightline})}
#'
#' \item{\code{color}: the hexadecimal name of the color of the point if R, G and B
#' fields exist (see \link[lidR:lascolor]{lascolor})}
#' }
#' @slot data data.table. a table representing the LAS data
#' @slot header list. A list of information contained in the las file header.
#' @slot crs A \link[sp:CRS]{CRS} object.
#' @seealso
#' \link[lidR:LAS]{LAS}
#' \link[lidR:readLAS]{readLAS}
#' @name LAS-class
#' @rdname LAS-class
#' @import data.table
#' @import methods
#' @include class-lasheader.r
#' @importClassesFrom sp CRS
#' @exportClass LAS
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAS",
  representation(
    data 	 = "data.table",
    header = "LASheader",
    crs = "CRS"
  )
)

setMethod("initialize", "LAS", function(.Object, data, header, crs, check)
{
  if(is.data.frame(data))
    data.table::setDT(data)

  if(!is.data.table(data))
    stop("Invalid parameter data in constructor.", call. = FALSE)

  if(nrow(data) == 0)
    stop("'data' is empty. No point found.", call. = FALSE)

  if (is(header, "LASheader"))
    header = as.list(header)

  if(is.list(header))
  {
    if (length(header) == 0)
    {
      header = rlas::header_create(data)
      check = FALSE
    }
  }
  else
    stop("Wrong header object provided.", call. = FALSE)

  header = rlas::header_update(header, data)

  if(check)
  {
    rlas::check_header(header)
    rlas::check_data(data)
    rlas::check_data_vs_header(header, data, hard = F)
  }

  header = LASheader(header)

  .Object@data   <- data
  .Object@header <- header

  if(is.na(crs@projargs))
    .Object@crs <- epsg2proj(get_epsg(header))
  else
    .Object@crs <- crs

  return(.Object)
})

#' Create a \code{LAS} object
#'
#' @param data a data.table containing the LiDAR data.
#' @param header a list containing the data from the header of a las file.
#' @param crs A \link[sp:CRS]{CRS} object.
#' @param check logical. consistency tests while building the object.
#' @return An object of class \code{LAS}
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' @export LAS
LAS <- function(data, header = list(), crs = sp::CRS(), check = TRUE) {return(new("LAS", data, header, crs, check))}

setMethod("show", "LAS", function(object)
{
  size <- format(utils::object.size(object), units = "auto")

  if("pulseID" %in% names(object@data))
    npu = data.table::uniqueN(object@data$pulseID)
  else
    npu = NA

  s   = area(object)
  npt = nrow(object@data)
  dpt = npt/s
  dpu = npu/s
  fie = names(object@data)
  ext = extent(object)
  phb = object@header@PHB

  cat("class        : LAS (", phb$`File Signature`, " v", phb$`Version Major`, ".", phb$`Version Minor`, ")\n", sep = "")
  cat("memory       :", size, "\n")
  cat("extent       :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("area         :", s, "m\u00B2 (convex hull)\n")
  cat("points       :", npt, "points, ", npu , "pulses\n")
  cat("density      :", round(dpt, 2), "points/m\u00B2, ", round(dpu, 2), "pulses/m\u00B2\n")
  cat("field names  :", fie, "\n")
  cat("coord. ref.  :", object@crs@projargs, "\n\n")
})

#' LiDAR data summary
#'
#' Print a summary of a LAS object
#'
#' @param object A LAS object
#' @param ... unused
#' @method summary LAS
#' @export
summary.LAS <- function(object, ...)
{
  print(object)
  print(object@header)
}

