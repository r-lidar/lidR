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
#' A LAS object contains the data and the header read in a .las file and
#' additional values computed on the fly during loading.
#'
#' A \code{LAS} object contains a \code{data.table} in the slot \code{@data} with
#' the data read from a \code{.las} file and other information computed during
#' data loading. The fields read from the las file are named:
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
#' A \code{LAS} object also contains a slot \code{@header} which contains the
#' header of the \code{.las} file. See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#' for more information.\cr
#'
#' @slot data data.table. a table representing the LAS data
#' @slot header list. A list of information contained in the las file header.
#' @seealso
#' \link[lidR:LAS]{LAS}
#' \link[lidR:readLAS]{readLAS}
#' @name LAS-class
#' @rdname LAS-class
#' @import data.table
#' @import magrittr
#' @import methods
#' @include lasheader-class.r
#' @exportClass LAS
#' @useDynLib lidR, .registration = TRUE
setClass(
  Class = "LAS",
  representation(
    data 	 = "data.table",
    header = "LASheader"
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

    header@data["Number of 1st return"] <- number_of[1]
    header@data["Number of 2nd return"] <- number_of[2]
    header@data["Number of 3rd return"] <- number_of[3]
    header@data["Number of 4th return"] <- number_of[4]
    header@data["Number of 5th return"] <- number_of[5]
  }

  header@data["Number of point records"] <- dim(data)[1]
  header@data["Min X"] <- min(data$X)
  header@data["Min Y"] <- min(data$Y)
  header@data["Min Z"] <- min(data$Z)
  header@data["Max X"] <- max(data$X)
  header@data["Max Y"] <- max(data$Y)
  header@data["Max Z"] <- max(data$Z)

  header@data["File Signature"] = "LASF"
  header@data["File Creation Day of Year"] <- strftime(Sys.time(), format = "%j") %>% as.numeric
  header@data["File Creation Year"] <- strftime(Sys.time(), format = "%Y") %>% as.numeric

  if("gpstime" %in% names(data)) # format 1 or 3
  {
    if(any(c("R", "G", "B") %in% names(data)))
    {
      header@data["Point Data Format ID"] = 3
      header@data["Point Data Record Length"] = 34
    }
    else
    {
      header@data["Point Data Format ID"] = 1
      header@data["Point Data Record Length"] = 28
    }
  }
  else # format 0 or 2
  {
    if(any(c("R", "G", "B") %in% names(data)))
    {
      header@data["Point Data Format ID"] = 2
      header@data["Point Data Record Length"] = 26
    }
    else
    {
      header@data["Point Data Format ID"] = 0
      header@data["Point Data Record Length"] = 20
    }
  }

  if(is.null(header@data[["Version Major"]]))
    header@data["Version Major"] = 1

  if(is.null(header@data[["Version Minor"]]))
    header@data["Version Minor"] = 2

  if(is.null(header@data[["X offset"]]) & is.null(header@data[["Y offset"]]) & is.null(header@data[["Z offset"]]))
  {
    header@data["X offset"] = header@data[["Min X"]]
    header@data["Y offset"] = header@data[["Min Y"]]
    header@data["Z offset"] = header@data[["Min Z"]]
    num = c(header@data[["Min X"]], header@data[["Min Y"]], header@data[["Min Z"]])
    lidRError("LDR12", what = "X Y and Z offsets", num = round(num,2), behaviour = warning)
  }

  if(is.null(header@data[["X scale factor"]]) & is.null(header@data[["Y scale factor"]]) & is.null(header@data[["Z scale factor"]]))
  {
    header@data["X scale factor"] = 0.01
    header@data["Y scale factor"] = 0.01
    header@data["Z scale factor"] = 0.01
    lidRError("LDR12", what = "X Y and Z scale factors", num = rep(0.01,3), behaviour = warning)
  }

  if(is.null(header@data[["X offset"]])) {
    header@data["X offset"] = header@data[["Min X"]]
    lidRError("LDR11", what = "X offset", num = round(header@data[["Min X"]],2), behaviour = warning)
  }

  if(is.null(header@data[["Y offset"]])) {
    header@data["Y offset"] = header@data[["Min Y"]]
    lidRError("LDR11", what = "Y offset", num = round(header@data[["Min Y"]],2), behaviour = warning)
  }

  if(is.null(header@data[["Z offset"]])) {
    header@data["Z offset"] = header@data[["Min Z"]]
    lidRError("LDR11", what = "Z offset", num = round(header@data[["Min Z"]],2), behaviour = warning)
  }

  if(is.null(header@data[["X scale factor"]])) {
    header@data["X scale factor"] = 0.01
    lidRError("LDR11", what = "X scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@data[["Y scale factor"]])) {
    header@data["Y scale factor"] = 0.01
    lidRError("LDR11", what = "Y scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@data[["Z scale factor"]])) {
    header@data["Z scale factor"] = 0.01
    lidRError("LDR11", what = "Z scale factor", num = 0.01, behaviour = warning)
  }

  if(is.null(header@data[["File Source ID"]]))
    header@data["File Source ID"] = 0

  if(is.null(header@data[["System Identifier"]]))
    header@data["System Identifier"] = "lidR"

  if(is.null(header@data[["Generating Software"]]))
    header@data["Generating Software"] = paste("lidR", utils::packageVersion("lidR"))

  if(is.null(header@data[["Header Size"]]))
    header@data["Header Size"] = 227

  if(is.null(header@data[["Offset to point data"]]))
    header@data["Offset to point data"] = 227

  if(is.null(header@data[["Project ID - GUID"]]))
    header@data["Project ID - GUID"] = 0

  if(is.null(header@data[["Global Encoding"]]))
    header@data["Global Encoding"] = 0

  header@data["Number of variable length records"] = length(header@data[["Variable Length Records"]])

  # Build returned object  ---------------------------------------------------

  .Object@data   <- data
  .Object@header <- header

  return(.Object)
})

#' Extract parts of a LAS object
#'
#' @param x object from which to extract element(s).
#' @param name A literal character string or a name (possibly backtick quoted).
setMethod("$", "LAS", function(x, name)
{
  if(name %in% names(x@data))
    return(as.numeric(unlist(x@data[,name,with=F])))
  else if(name %in% slotNames(x))
    return(slot(x, name))
  else if(name %in% names(x@header@data))
    return(x@header@data[name])
})

#' Create a \code{LAS} object
#'
#' @param data a data.table containing the LiDAR data.
#' @param header a list containing the data from the header of a las file.
#' @param check logical. constitency tests while building the object.
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

  s   = lasarea(object)
  npt = nrow(object@data)
  dpt = npt/s
  dpu = npu/s
  fie = names(object@data)
  ext = extent(object)

  cat("class        : LAS\n")
  cat("memory       :", size, "\n")
  cat("extent       :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
  cat("area         :", s, "m\u00B2 (convex hull)\n")
  cat("points       :", npt, "points\n")
  cat("pulses       :", npu , "pulses\n")
  cat("point density:", round(dpt, 2), "points/m\u00B2\n")
  cat("pulse density:", round(dpu, 2), "pulses/m\u00B2\n")
  cat("fields       :", length(fie), "\n")
  cat("field names  :", fie, "\n\n")

  print(object@header)
})

