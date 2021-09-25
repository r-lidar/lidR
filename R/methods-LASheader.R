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

#' Create a \code{LASheader} object
#'
#' Creates a  \code{LASheader} object either from a raw \code{list} containing all the
#' elements named according to the \code{rlas} package or creates a header from a \code{data.frame}
#' or \code{data.table} containing a point cloud. In the latter case it will generate a header
#' according to the data using \link[rlas:header_create]{rlas::header_create()}. It will
#' guess the LAS file format, the point data format, and initialize the scale factors and offsets,
#' but these may not suit a user's needs. Users are advised to
#' manually modify the results to fit their specific needs.
#'
#' @param data a list containing the data from the header of a LAS file. Can also be
#' a \code{data.frame} or \code{data.table}
#' @return An object of class \code{LASheader}
#' @examples
#' data = data.frame(X = c(339002.889, 339002.983, 339002.918),
#'                   Y = c(5248000.515, 5248000.478, 5248000.318),
#'                   Z = c(975.589, 974.778, 974.471),
#'                   gpstime = c(269347.28141, 269347.28142, 269347.28143),
#'                   Intensity = c(82L, 54L, 27L),
#'                   ReturnNumber = c(1L, 1L, 2L),
#'                   NumberOfReturns = c(1L, 1L, 2L),
#'                   ScanDirectionFlag = c(1L, 1L, 1L),
#'                   EdgeOfFlightline = c(1L, 0L, 0L),
#'                   Classification = c(1L, 1L, 1L),
#'                   ScanAngleRank = c(-21L, -21L, -21L),
#'                   UserData = c(32L, 32L, 32L),
#'                   PointSourceID = c(17L, 17L, 17L))
#'
#' header = LASheader(data)
#' header
#'
#' # XYZ values are given with 3 decimals. This was not inferred by the
#' # function so we changed it manually
#' # (Note: from package rlas 1.4.1 this is now inferred properly in most cases)
#' header@PHB[["X scale factor"]] <- 0.001
#' header@PHB[["Y scale factor"]] <- 0.001
#' header@PHB[["Z scale factor"]] <- 0.001
#'
#' # Record an EPSG code
#' epsg(header) <- 32618
#' header
#'
#' las <- LAS(data, header)
#' las
#'
#' # The function inferred a LAS 1.2 format 1 which is correct
#' # Upgrade to LAS 1.4 for the example
#' header@VLR <- list() # Erase VLR previously written
#' header@PHB[["Global Encoding"]][["WKT"]] <- TRUE
#' header@PHB[["Version Minor"]] <- 4L
#' header@PHB[["Header Size"]] <- 375L
#' header@PHB[["Offset to point data"]] <- 375L
#' wkt(header) <- sf::st_crs("EPSG:32618")$wkt
#' header
#' las1.4 <- LAS(data, header)
#' las1.4
#' @export
LASheader <- function(data = list()) {return(new("LASheader", data))}


#' Transform to a list
#'
#' Functions to construct, coerce and check for both kinds of R lists.
#'
#' @param x A LASheader object
#' @param ... unused
#' @method as.list LASheader
#' @export
as.list.LASheader <- function(x, ...)
{
  PHB  <- x@PHB
  VLR  <- list(`Variable Length Records` = x@VLR)
  EVLR <- list(`Extended Variable Length Records` = x@EVLR)
  return(c(PHB, VLR, EVLR))
}
