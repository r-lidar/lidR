# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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


catalog_clip_inpoly = function(catalog, xpoly, ypoly, ofile)
{
  xmin <- min(xpoly)
  xmax <- max(xpoly)
  ymin <- min(ypoly)
  ymax <- max(ypoly)
  xc   <- (xmax + xmin)/2
  yc   <- (ymax + ymin)/2
  w    <- xmax - xmin
  h    <- ymax - ymin

  filter <- paste("-inside", xmin, ymin, xmax, ymax)
  index  <- catalog_index(catalog, xc, yc, w, h, buffer = 1)
  files  <- unlist(index$filename)

  streaming_clip(files, filter, ofile, xpoly, ypoly)
}

catalog_clip_rect = function(catalog, xmin, ymin, xmax, ymax, ofile)
{
  xc <- (xmax + xmin)/2
  yc <- (ymax + ymin)/2
  w  <- xmax - xmin
  h  <- ymax - ymin

  filter <- paste("-inside", xmin, ymin, xmax, ymax)
  index  <- catalog_index(catalog, xc, yc, w, h, buffer = 1)
  files  <- unlist(index$filename)

  streaming_clip(files, filter, ofile)
}

catalog_clip_circ = function(catalog, xcenter, ycenter, radius, ofile)
{
  w  <- 2*radius

  filter <- paste("-inside_circle", xcenter, ycenter, radius)
  index  <- catalog_index(catalog, xc, yc, w, w, buffer = 1)
  files  <- unlist(index$filename)

  streaming_clip(files, filter, ofile)
}