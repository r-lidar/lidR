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

#' An S4 class to represent the header of .las or .laz file
#'
#' An S4 class to represent the header of .las or .laz file according to the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format specifications}.
#' A \code{LASheader} object contains a \code{list} in the slot \code{@PHB} with
#' the data read from the Public Header Block and \code{list} in the slot \code{@VLR} with
#' the data read from the Variable Lenght Records
#'
#' @slot PHB list. Represents the Public Header Block
#'
#' @slot VLR list. Represents the Variable Length Records
#'
#' @exportClass LASheader
setClass(
  Class = "LASheader",
  representation(PHB = "list", VLR = "list")
)

setMethod("initialize", "LASheader", function(.Object, data = list())
{
  assertive::assert_is_list(data)

  vlr <- list()
  if (!is.null(data$`Variable Length Records`))
    vlr <- data$`Variable Length Records`

  .Object@PHB <- data
  .Object@PHB$`Variable Length Records` <- NULL
  .Object@VLR <- vlr

  return(.Object)
})
