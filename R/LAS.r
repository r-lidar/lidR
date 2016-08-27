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



#' Create a \code{LAS} object
#'
#' When a \code{LAS} object is created sevral things are computed on the fly in addition
#' to the data inputed in the constructor. See \link[lidR:LAS-class]{LAS-class}.
#'
#' @param data a data.table containing the LiDAR data.
#' @param header a list containing the data from the header of a las file.
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' summary(lidar)
#' @seealso
#' \link[lidR:LAS]{Class LAS}
#' \link[lidR:getData]{getData}
#' \link[lidR:summary]{summary}
#' @export LAS
LAS <- function(data, header = list()) {return(new("LAS", data, header))}
