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
#' @exportClass LAS
#' @useDynLib lidR
setClass(
	Class = "LAS",
	representation(
		data 	 = "data.table",
		header = "list"
	)
)

setMethod("initialize", "LAS",
	function(.Object, data, header = list())
	{
	  if(is.data.frame(data))
	    data.table::setDT(data)

	  if(!is.data.table(data))
	    lidRError("LDR1")

	  if(nrow(data) == 0)
	    lidRError("LDR9")

	  # Check if the data are valid. Else: warning -------------------------------

	  lascheck(data)

	  # Update header ------------------------------------------------------------

	  if("ReturnNumber" %in% names(data))
	  {
	    number_of <- fast_table(data$ReturnNumber, 5L)

	    header["Number of 1st return"] <- number_of[1]
	    header["Number of 2nd return"] <- number_of[2]
	    header["Number of 3rd return"] <- number_of[3]
	    header["Number of 4th return"] <- number_of[4]
	    header["Number of 5th return"] <- number_of[5]
	  }

	  header["Number of point records"] <- dim(data)[1]
	  header["Min X"] <- min(data$X)
	  header["Min Y"] <- min(data$Y)
	  header["Min Z"] <- min(data$Z)
	  header["Max X"] <- max(data$X)
	  header["Max Y"] <- max(data$Y)
	  header["Max Z"] <- max(data$Z)

	  # Build returned object  ---------------------------------------------------

	  .Object@data   <- data
	  .Object@header <- header

	  return(.Object)
	}
)

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
  else if(name %in% names(x@header))
    return(x@header[name])
})

#' Create a \code{LAS} object
#'
#' @param data a data.table containing the LiDAR data.
#' @param header a list containing the data from the header of a las file.
#' @return An object of class \code{LAS}
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' \link[lidR:summary]{summary}
#' @export LAS
LAS <- function(data, header = list()) {return(new("LAS", data, header))}

setMethod("show", "LAS",
  function(object)
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
    x   = object@header

    cat("class        : LAS\n")
    cat("memory       :", size, "\n")
    cat("extent       :", ext@xmin, ",", ext@xmax, ",", ext@ymin, ",", ext@ymax, "(xmin, xmax, ymin, ymax)\n")
    cat("area         :", s, "m^2\n")
    cat("points       :", npt, "points\n")
    cat("pulses       :", npu , "pulses\n")
    cat("point density:", round(dpt, 2), "points/m^2\n")
    cat("pulse density:", round(dpu, 2), "pulses/m^2\n")
    cat("fields       :", length(fie), "\n")
    cat("field names  :", fie, "\n\n")

    cat("File signature:          ", x$`File Signature`, "\n")
    cat("File source ID:          ", x$`File Source ID`, "\n")
    cat("Global encoding:         ", x$`Global Encoding`, "\n")
    cat("Project ID - GUID:       ", x$`Project ID - GUID`, "\n")
    cat("Version:                  ", x$`Version Major`, ".", x$`Version Minor`, "\n", sep = "")
    cat("System identifier:       ", x$`System Identifier`, "\n")
    cat("Generating software:     ", x$`Generating Software`, "\n")
    cat("File creation d/y:        ", x$`File Creation Day of Year`, "/", x$`File Creation Year`, "\n", sep = "")
    cat("header size:             ", x$`Header Size`, "\n")
    cat("Offset to point data:    ", x$`Offset to point data`, "\n")
    cat("Num. var. length record: ", x$`Number of variable length records`, "\n")
    cat("Point data format:       ", x$`Point Data Format ID`, "\n")
    cat("Point data record length:", x$`Point Data Record Length`, "\n")
    cat("Num. of point records:   ", x$`Number of point records`, "\n")
    cat("Num. of points by return:", x$`Number of 1st return`, x$`Number of 2nd return`, x$`Number of 3rd return`, x$`Number of 4th return`, x$`Number of 5th return`, "\n")
    cat("Scale factor X Y Z:      ", x$`X scale factor`, x$`Y scale factor`, x$`Z scale factor`, "\n")
    cat("Offset X Y Z:            ", x$`X offset`, x$`Y offset`, x$`Z offset`, "\n")
    cat("min X Y Z:               ", x$`Min X`, x$`Min Y`, x$`Min Z`, "\n")
    cat("max X Y Z:               ", x$`Max X`, x$`Max Y`, x$`Max Z`, "\n")

    n = length(x$`Variable Length Records`)

    if(n == 0)
    {
      cat("Variable length records:  void\n")
      return(invisible())
    }

    cat("Variable length records: \n")

    for(i in 1:n)
    {
      vlr = x$`Variable Length Records`[[i]]

      cat("   Variable length record", i, "of", n, "\n")
      cat("       Reserved:            ", vlr$reserved, "\n")
      cat("       User ID:             ", vlr$`user ID`, "\n")
      cat("       record ID:           ", vlr$`record ID`, "\n")
      cat("       Length after header: ", vlr$`length after header`, "\n")
      cat("       Description:         ", vlr$description, "\n")

      if(vlr$`record ID` == 34735)
      {
        cat("       Tags:\n")
        lapply(vlr[[6]], function(xx)
        {
          cat("          Key", xx$key, "tiff_tag_location", xx$`tiff tag location`, "count", xx$count, "value offset", xx$`value offset`, "\n")
        })
      }
      else if(vlr$`record ID` == 34736)
      {
        cat("       data:                ", vlr[[6]], "\n")
      }
      else if(vlr$`record ID` == 34737)
      {
        cat("       data:                ", vlr[[6]], "\n")
      }
    }

    return(invisible())
  }
)

