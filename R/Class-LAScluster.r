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


#' An S4 class to represent an arbitrary region in the catalog.
#'
#' @keywords internal
setClass(
  Class = "LAScluster",
  representation(
    center = "list",
    bbox   = "list",
    bbbox  = "list",
    width   = "numeric",
    height = "numeric",
    buffer = "numeric",
    shape  = "numeric",
    select = "character",
    filter = "character",
    wkt    = "character",
    files  = "character",
    name   = "character",
    save   = "character"
  )
)

setMethod("initialize", "LAScluster", function(.Object, center, width, height, buffer, shape, files, name, wkt)
{
  hw = width/2
  hh = height/2
  xc = center$x
  yc = center$y
  bu = buffer

  .Object@center <- center
  .Object@bbox   <- list(xmin = xc - hw, xmax = xc + hw, ymin = yc - hh, ymax = yc + hh)
  .Object@bbbox  <- list(xmin = xc - hw - bu, xmax = xc + hw + bu, ymin = yc - hh - bu, ymax = yc + hh + bu)
  .Object@width  <- width  + 2*buffer
  .Object@height <- height + 2*buffer
  .Object@buffer <- buffer
  .Object@shape  <- shape
  .Object@name   <- name
  .Object@files  <- files
  .Object@save   <- ""
  .Object@wkt    <- ""

  if (shape == LIDRCIRCLE)
    .Object@filter = paste("-inside_circle", xc, yc, hw + buffer)
  else if (shape == LIDRRECTANGLE)
    .Object@filter = paste("-inside", .Object@bbbox$xmin, .Object@bbbox$ymin, .Object@bbbox$xmax, .Object@bbbox$ymax)
  else
    stop("Something went wrong internally initializing a cluster. Process aborted.")

  return(.Object)
})