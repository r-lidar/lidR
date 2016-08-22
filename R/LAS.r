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



#' Load a las file and create a 'LAS' object
#'
#' Methods to read and create a \code{LAS} object from a vector of .las filename(s)
#'
#' Methods to read and create a \code{LAS} object from a vector of .las filename(s).
#' The option fields allows selection of fields to be loaded. Removing redundant fields
#' saves memory. The option '\code{minimal}' loads only X,Y,Z and gpstime allowing
#' pulseID and flightlineID to be computed. The option '\code{standard}' loads all fields
#' apart from UserDate, EdgeofFlighline and PointSourceID. The option '\code{all}' loads everything.
#'
#' @param input character or Catalog object. Filename of .las file. Use \link[base:c]{c()} to concatenate several files.
#' If input is a \link[lidR:Catalog-class]{Catalog} object, all the .las file in the catalog will be loaded.
#' @param fields character. Can be \code{"minimal"}, \code{"standard"}, \code{"all"}. Default is standard. See details.
#' @param \dots Unused
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' getData(lidar)
#' summary(lidar)
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' \link[lidR:getData]{getData}
#' \link[lidR:summary]{summary}
#' @export readLAS
LAS <- function(input, fields = "standard", ...) {return(new("LAS", input, fields, ...))}
