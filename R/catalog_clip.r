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


catalog_clip_poly = function(catalog, xpoly, ypoly, ofile)
{
  xmin <- min(xpoly)
  xmax <- max(xpoly)
  ymin <- min(ypoly)
  ymax <- max(ypoly)
  xc   <- (xmax + xmin)/2
  yc   <- (ymax + ymin)/2
  w    <- xmax - xmin + 1
  h    <- ymax - ymin + 1

  cluster  <- catalog_index(catalog, xc, yc, w, h, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  header = rlas::readlasheader(cluster@files[1])
  data   = rlas:::streamlasdata_inpoly(cluster@files, xpoly, ypoly, filter = cluster@filter, ofile = ofile)

  if (is.null(data))
    return (invisible())

  return(LAS(data, header))
}

catalog_clip_rect = function(catalog, xmin, ymin, xmax, ymax, ofile)
{
  xc <- (xmax + xmin)/2
  yc <- (ymax + ymin)/2
  w  <- xmax - xmin
  h  <- ymax - ymin

  cluster  <- catalog_index(catalog, xc, yc, w, h, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  header = rlas::readlasheader(cluster@files[1])
  data   = rlas:::streamlasdata(cluster@files, ofile = ofile, filter = cluster@filter)

  if (nrow(data) == 0)
    return (invisible())

  return(LAS(data, header))
}

catalog_clip_circ = function(catalog, xcenter, ycenter, radius, ofile)
{
  cluster  <- catalog_index(catalog, xcenter, ycenter, 2*radius, NULL, 0, "ROI")[[1]]

  if (is.null(cluster))
    return(invisible())

  header = rlas::readlasheader(cluster@files[1])
  data   = rlas:::streamlasdata(cluster@files, ofile = ofile, filter = cluster@filter)

  if (nrow(data) == 0)
    return (invisible())

  return(LAS(data, header))
}