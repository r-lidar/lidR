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

setClass(
  Class = "LAScluster", contains = "Spatial",
  representation(
    center = "list",
    bbbox  = "matrix",
    width  = "numeric",
    height = "numeric",
    buffer = "numeric",
    shape  = "numeric",
    select = "character",
    filter = "character",
    wkt    = "character",
    files  = "character",
    name   = "character",
    save   = "character",
    alt_dir = "character"
  )
)

setMethod("initialize", "LAScluster", function(.Object, center, width, height, buffer, shape, files, name, wkt, proj4string)
{
  hw = width/2
  hh = height/2
  xc = center$x
  yc = center$y

  .Object@center <- center
  .Object@bbox   <- matrix(c(xc - hw, yc - hh, xc + hw, yc + hh), ncol = 2)
  .Object@bbbox  <- .Object@bbox + buffer * matrix(c(-1, -1, +1, +1), ncol = 2)
  .Object@width  <- width  + 2 * buffer
  .Object@height <- height + 2 * buffer
  .Object@buffer <- buffer
  .Object@shape  <- shape
  .Object@name   <- name
  .Object@files  <- files
  .Object@save   <- ""
  .Object@wkt    <- ""
  .Object@proj4string <- proj4string

  if (shape == LIDRCIRCLE)
    .Object@filter = paste("-inside_circle", xc, yc, hw + buffer)
  else if (shape == LIDRRECTANGLE)
    .Object@filter = paste("-inside", .Object@bbbox[1], .Object@bbbox[2], .Object@bbbox[3], .Object@bbbox[4])
  else
    stop("Something went wrong internally initializing a cluster. Process aborted.")

  return(.Object)
})
