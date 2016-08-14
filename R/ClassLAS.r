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



#' An S4 class to represent a LAS dataset.
#'
#' An S4 class to represent a LAS dataset. It contains the data, the header and additional
#' values computed during the loading.
#'
#' A \code{LAS} object contains a \code{data.table} in the slot \code{@data} with the data
#' read from a \code{.las} file and other information computed during data loading. The
#' fields read from the las file are named:
#' \itemize{
#' \item{\code{X Y Z}}
#' \item{\code{Intensity}}
#' \item{\code{ReturnNumber}}
#' \item{\code{NumberOfReturns}}
#' \item{\code{ScanDirectionFlag}}
#' \item{\code{EdgeofFlightline}}
#' \item{\code{Classification}}
#' \item{\code{ScanAngle}}
#' \item{\code{UserData}}
#' \item{\code{PointSourceID}}
#' }
#' When a \code{LAS} object is built, two other variables are computed in the \code{data.table}:
#' \itemize{
#' \item{\code{pulseID}: }{a unique identifying number for each pulse so the beam origin of each point is known}
#' \item{\code{flightlineID}: }{a unique identifying number for the flightline so the flightline origin of each point is known}}
#' A \code{LAS} object contains other information in slots \code{@area}, \code{@pointDensity} and \code{@pulseDensity}:
#' \itemize{
#' \item{\code{area}: }{is computed with a convex hull. It is only an approximation if the shape of the data is not convex.}
#' \item{\code{points} and \code{pulse density}: }{are computed using the computed area. Also an approximation if the data are not convex}
#' }
#' A \code{LAS} object also contains a slot \code{@header} which contains the header of the \code{.las} file.
#' See the public documentation of \code{.las} file format for more information.
#'
#' @slot data data.table. a table representing the LAS data
#' @slot area numeric. The area of the dataset computed with a convex hull
#' @slot pointDensity numeric. The point density of the dataset
#' @slot pulseDensity numeric. The pulse density of the dataset
#' @slot header list. A list of information contained in the las file header.
#' @seealso
#' \link[lidR:LAS]{LAS}
#' @name LAS-class
#' @rdname LAS-class
#' @exportClass LAS
#' @import dtplyr
#' @importFrom methods new
#' @importFrom grDevices rgb
setClass(
	Class = "LAS",
	representation(
		data 					= "data.table",
		area 					= "numeric",
		pointDensity 	= "numeric",
		pulseDensity 	= "numeric",
		header        = "list"
	)
)

#' @importFrom data.table is.data.table setorder
setMethod("initialize", "LAS",
	function(.Object, input, fields = "standard", ...)
	{
	  gpstime <- R <- G <- B <- NULL

	  if(class(input)[1] == "Catalog")
	    input = input@headers$filename

	  if(is.character(input))
	  {
	    .Object@data   = readLASdata(input)
	    .Object@header = readLASheader(input)
	  }
	  else if(is.data.table(input))
	  {
	    .Object@data = input
	  }
	  else
	    lidRError("LDR1")

	  negvalues = sum(.Object@data$Z < 0)
	  class0    = sum(.Object@data$Classification == 0)

	  if(negvalues > 0)
	    lidRError("LDR2", number = negvalues, behaviour = warning)

	  if(class0 > 0)
	    lidRError("LDR3", number = class0, behaviour = warning)

	  if("gpstime" %in% names(.Object@data))
	  {
  	  setorder(.Object@data, gpstime)

  	  .Object@area <- area(.Object)

  	  if(is.null(.Object@data$pulseID))
  	    .Object@data$pulseID <- .IdentifyPulse(.Object@data$ReturnNumber)

  	  if(is.null(.Object@data$flightlineID))
  	    .Object@data$flightlineID <- .IdentifyFlightlines(.Object@data$gpstime)

  	  .Object@pulseDensity <- .pulseDensity(.Object)
	  }
	  else
	    lidRError("LDR4", behaviour = warning)

	  if(sum(c("R", "G", "B") %in% names(.Object@data)) == 3)
	  {
	    if(is.null(.Object@data$color))
  	    .Object@data$color <- .Object@data %$% grDevices::rgb(R/255, G/255, B/255)
	  }

	  .Object@pointDensity <- .pointDensity(.Object)

	  return(.Object)
	}
)

# Internal functions

setGeneric(".pointDensity", function(obj){standardGeneric(".pointDensity")})

#' @importFrom magrittr %>% divide_by
setMethod(".pointDensity", "LAS",
	function(obj)
	{
		d = obj@data %>% nrow %>% divide_by(obj@area) %>% round(2)
		return(d)
	}
)

setGeneric(".pulseDensity", function(obj){standardGeneric(".pulseDensity")})

#' @importFrom magrittr %>% divide_by
setMethod(".pulseDensity", "LAS",
	function(obj)
	{
		d = obj@data$pulseID %>% n_distinct %>% divide_by(obj@area) %>% round(2)
		return(d)
	}
)

#' @importFrom dplyr lag
.IdentifyPulse = function(return.number)
{
  boo = dplyr::lag(return.number) >= return.number
  boo[1] = TRUE
  return(cumsum(boo))
}

#' @importFrom dplyr lag
.IdentifyFlightlines = function(time, t = 30)
{
  boo = time - dplyr::lag(time) > t
  boo[1] = TRUE
  return(cumsum(boo))
}


