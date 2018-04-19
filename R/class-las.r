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
#' @import magrittr
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

setMethod("initialize", "LAS", function(.Object, data, header, check)
{
  if(is.data.frame(data))
    data.table::setDT(data)

  if(!is.data.table(data))
    lidRError("LDR1")

  if(nrow(data) == 0)
    lidRError("LDR9")

  if(!is(header, "LASheader"))
    header = LASheader(header)

  # Check if the data are valid. Else: warning -------------------------------

  if(check)
    lascheck(data, header, hard = F)

  # Update header ------------------------------------------------------------

  if("ReturnNumber" %in% names(data))
  {
    number_of <- fast_table(data$ReturnNumber, 5L)

    header@PHB["Number of 1st return"] <- number_of[1]
    header@PHB["Number of 2nd return"] <- number_of[2]
    header@PHB["Number of 3rd return"] <- number_of[3]
    header@PHB["Number of 4th return"] <- number_of[4]
    header@PHB["Number of 5th return"] <- number_of[5]
  }

  header@PHB["Number of point records"] <- dim(data)[1]
  header@PHB["Min X"] <- min(data$X)
  header@PHB["Min Y"] <- min(data$Y)
  header@PHB["Min Z"] <- min(data$Z)
  header@PHB["Max X"] <- max(data$X)
  header@PHB["Max Y"] <- max(data$Y)
  header@PHB["Max Z"] <- max(data$Z)

  header@PHB["File Signature"] = "LASF"
  header@PHB["File Creation Day of Year"] <- strftime(Sys.time(), format = "%j") %>% as.numeric
  header@PHB["File Creation Year"] <- strftime(Sys.time(), format = "%Y") %>% as.numeric

  if("gpstime" %in% names(data)) # format 1 or 3
  {
    if(any(c("R", "G", "B") %in% names(data)))
    {
      header@PHB["Point Data Format ID"] = 3
      header@PHB["Point Data Record Length"] = 34
    }
    else
    {
      header@PHB["Point Data Format ID"] = 1
      header@PHB["Point Data Record Length"] = 28
    }
  }
  else # format 0 or 2
  {
    if(any(c("R", "G", "B") %in% names(data)))
    {
      header@PHB["Point Data Format ID"] = 2
      header@PHB["Point Data Record Length"] = 26
    }
    else
    {
      header@PHB["Point Data Format ID"] = 0
      header@PHB["Point Data Record Length"] = 20
    }
  }

  if(is.null(header@PHB[["Version Major"]]))
    header@PHB["Version Major"] = 1

  if(is.null(header@PHB[["Version Minor"]]))
    header@PHB["Version Minor"] = 2

  if(is.null(header@PHB[["X offset"]]) & is.null(header@PHB[["Y offset"]]) & is.null(header@PHB[["Z offset"]]))
  {
    header@PHB["X offset"] = header@PHB[["Min X"]]
    header@PHB["Y offset"] = header@PHB[["Min Y"]]
    header@PHB["Z offset"] = header@PHB[["Min Z"]]
    num = c(header@PHB[["Min X"]], header@PHB[["Min Y"]], header@PHB[["Min Z"]])
    lidRError("LDR12", what = "X Y and Z offsets", num = round(num,2), behaviour = warning)
  }

  if(is.null(header@PHB[["X scale factor"]]) & is.null(header@PHB[["Y scale factor"]]) & is.null(header@PHB[["Z scale factor"]]))
  {
    header@PHB["X scale factor"] = 0.01
    header@PHB["Y scale factor"] = 0.01
    header@PHB["Z scale factor"] = 0.01
    lidRError("LDR12", what = "X Y and Z scale factors", num = rep(0.01,3), behaviour = warning)
  }

  if(is.null(header@PHB[["X offset"]])) {
    header@PHB["X offset"] = header@PHB[["Min X"]]
    lidRError("LDR11", what = "X offset", num = round(header@PHB[["Min X"]],2), behaviour = warning)
  }

  if(is.null(header@PHB[["Y offset"]])) {
    header@PHB["Y offset"] = header@PHB[["Min Y"]]
    lidRError("LDR11", what = "Y offset", num = round(header@PHB[["Min Y"]],2), behaviour = warning)
  }

  if(is.null(header@PHB[["Z offset"]])) {
    header@PHB["Z offset"] = header@PHB[["Min Z"]]
    lidRError("LDR11", what = "Z offset", num = round(header@PHB[["Min Z"]],2), behaviour = warning)
  }

  if(is.null(header@PHB[["X scale factor"]])) {
    header@PHB["X scale factor"] = 0.01
    lidRError("LDR11", what = "X scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@PHB[["Y scale factor"]])) {
    header@PHB["Y scale factor"] = 0.01
    lidRError("LDR11", what = "Y scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@PHB[["Z scale factor"]])) {
    header@PHB["Z scale factor"] = 0.01
    lidRError("LDR11", what = "Z scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@PHB[["File Source ID"]]))
    header@PHB["File Source ID"] = 0

  if(is.null(header@PHB[["System Identifier"]]))
    header@PHB["System Identifier"] = "lidR"

  if(is.null(header@PHB[["Generating Software"]]))
    header@PHB["Generating Software"] = paste("lidR", utils::packageVersion("lidR"))

  if(is.null(header@PHB[["Header Size"]]))
    header@PHB["Header Size"] = 227

  if(is.null(header@PHB[["Offset to point data"]]))
    header@PHB["Offset to point data"] = 227

  if(is.null(header@PHB[["Project ID - GUID"]]))
    header@PHB["Project ID - GUID"] = 0

  if(is.null(header@PHB[["Global Encoding"]]))
    header@PHB["Global Encoding"] = 0

  header@PHB["Number of variable length records"] = length(header@PHB[["Variable Length Records"]])

  # Build returned object  ---------------------------------------------------

  .Object@data   <- data
  .Object@header <- header
  .Object@crs    <- epsg2proj(get_epsg(header))

  return(.Object)
})

#' Create a \code{LAS} object
#'
#' @param data a data.table containing the LiDAR data.
#' @param header a list containing the data from the header of a las file.
#' @param check logical. consistency tests while building the object.
#' @return An object of class \code{LAS}
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' @export LAS
LAS <- function(data, header = list(), check = TRUE) {return(new("LAS", data, header, check))}

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

