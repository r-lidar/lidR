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
#' @param data a list containing the data from the header of a las file.
#' @return An object of class \code{LASheader}
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
