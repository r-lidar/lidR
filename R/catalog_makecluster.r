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

catalog_makecluster = function(ctg, res, buffer, by_file, size = CATALOGOPTIONS("tiling_size"))
{
  name <- NULL

  if (by_file)
  {
    X = ctg@data[, c("Min X", "Max X", "Min Y", "Max Y")]
    names(X) = c("xleft", "xright", "ybottom", "ytop")
    X$byfile = TRUE
  }
  else
  {
    # dimension of the clusters (width = height)
    width = ceiling(size/res) * res

    verbose("Computing the bounding box of the catalog...")

    # Bounding box of the catalog
    bbox = with(ctg@data, c(min(`Min X`), min(`Min Y`), max(`Max X`), max(`Max Y`)))

    # Buffer around the bbox as a multiple of the resolution
    buffered_bbox = bbox + c(-res, -res, +res, +res)
    buffered_bbox = round_any(buffered_bbox, res)
    buffered_bbox = buffered_bbox + c(-res, -res, +res, +res)

    verbose("Creating a set of cluster for the catalog...")

    # Generate coordinates of sub bounding boxes
    xleft   = seq(buffered_bbox[1], buffered_bbox[3], width)
    ybottom = seq(buffered_bbox[2], buffered_bbox[4], width)

    X = expand.grid(xleft = xleft, ybottom = ybottom)
    data.table::setDT(X)

    X$xright = X$xleft + width
    X$ytop   = X$ybottom + width

    X$byfile = FALSE
  }

  X$xleftbuff   = X$xleft - buffer
  X$ybottombuff = X$ybottom - buffer
  X$xrightbuff  = X$xright + buffer
  X$ytopbuff    = X$ytop + buffer
  X$xcenter     = (X$xleft + X$xright)/2
  X$ycenter     = (X$ybottom + X$ytop)/2
  X$name        = 1:nrow(X)

  # Remove cluster outside the catalog
  if (!by_file)
  {
    queries = suppressWarnings(catalog_index(ctg, X$xcenter, X$ycenter, width/2, width/2, buffer, X$name))
    X = X[name %in% names(queries)]
  }

  # Plot the pattern
  xrange = c(min(X$xleft), max(X$xright))
  yrange = c(min(X$ybottom), max(X$ytop))
  title  = "Pattern of clusters"
  plot.LAScatalog(ctg, y = FALSE, main = title, xlim = xrange, ylim = yrange)
  with(X, graphics::rect(xleft, ybottom, xright, ytop, border = "red"))

  if (buffer > 0)
    with(X, graphics::rect(xleftbuff, ybottombuff, xrightbuff, ytopbuff, border = "darkgreen", lty = "dotted"))

  return(X)
}