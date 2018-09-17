# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' An S4 class to represent a .las or .laz file
#'
#' Class LAS is the representation of a las/laz file according to the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format specifications}.
#'
#' A \code{LAS} object inherit a \link[sp:Spatial-class]{Spatial} object from \code{sp}. Thus it is
#' a \code{Spatial} object plus a \code{data.table} with the data read from  a \code{las/laz} file and
#' a \link[lidR:LASheader-class]{LASheader} (see the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#' for more information). las files being normalised the table of attributes read from the las/laz file
#' is also normalized. Columns are named:
#' \itemize{
#' \item{\code{X} (numeric)}
#' \item{\code{Y} (numeric)}
#' \item{\code{Z} (numeric)}
#' \item{\code{Intensity} (integer)}
#' \item{\code{ReturnNumber} (integer)}
#' \item{\code{NumberOfReturns} (integer)}
#' \item{\code{ScanDirectionFlag} (integer)}
#' \item{\code{EdgeOfFlightline} (integer)}
#' \item{\code{Classification} (integer)}
#' \item{\code{Synthetic_flag} (logical)}
#' \item{\code{Keypoint_flag} (logical)}
#' \item{\code{Withheld_flag} (logical)}
#' \item{\code{ScanAngle} (integer)}
#' \item{\code{UserData} (integer)}
#' \item{\code{PointSourceID} (integer)}
#' }
#'
#' @section Extends:
#' Class  \link[sp:Spatial-class]{Spatial}, directly.
#'
#' @slot bbox Object of class \code{matrix}, with bounding box
#'
#' @slot proj4string Object of class \link[sp:CRS-class]{CRS}, projection string
#'
#' @slot data Object of class \link[data.table:data.table]{data.table}. Point cloud data accoridng to the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#'
#' @slot header Object of class \link[lidR:LASheader-class]{LASheader}. las file header accoridng to the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}
#'
#' @import data.table
#' @import methods
#' @importClassesFrom sp Spatial
#'
#' @include Class-LASheader.r
#' @export
#' @useDynLib lidR, .registration = TRUE
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' @seealso
#' \link{readLAS}
setClass(
  Class = "LAS", contains = "Spatial",
  representation(data = "data.table", header = "LASheader")
)

setMethod("initialize", "LAS", function(.Object)
{
  x      <- numeric(0)
  data   <- data.table::data.table(X = x, Y = x, Z = x)
  header <- suppressWarnings(rlas::header_create(data))
  header$`System Identifier` <- "lidR R package"
  header$`Generating Software` <- "lidR R package"
  header$`Min X` <- 0
  header$`Max X` <- 0
  header$`Min Y` <- 0
  header$`Max Y` <- 0
  header$`Min Z` <- 0
  header$`Max Z` <- 0
  header$`X offset` <- 0
  header$`Y offset` <- 0
  header$`Z offset` <- 0

  .Object@bbox        <- matrix(0, 2, 2)
  .Object@proj4string <- sp::CRS()
  .Object@header      <- LASheader(header)
  .Object@data        <- data

  return(.Object)
})

