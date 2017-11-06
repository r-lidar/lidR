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
  xmin <- ymin <- xmax <- ymax <- 0

  if (by_file)
  {
    xmin = ctg@data$`Min X`
    xmax = ctg@data$`Max X`
    ymin = ctg@data$`Min Y`
    ymax = ctg@data$`Max Y`
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
    xmin = seq(buffered_bbox[1], buffered_bbox[3], width)
    ymin = seq(buffered_bbox[2], buffered_bbox[4], width)

    xmin = xmin[xmin < max(ctg@data$`Max X`)]
    ymin = ymin[ymin < max(ctg@data$`Max Y`)]

    X = expand.grid(xmin = xmin, ymin = ymin)

    xmin = X$xmin
    ymin = X$ymin
    xmax = xmin + width
    ymax = ymin + width
  }

  xcenter = (xmin + xmax)/2
  ycenter = (ymin + ymax)/2
  width   = xmax - xmin
  height  = ymax - ymin
  names   = paste0("ROI", 1:length(xcenter))

  clusters = suppressWarnings(catalog_index(ctg, xcenter, ycenter, width, height, buffer, names))

  # Plot the pattern
  xrange = c(min(xmin), max(xmax))
  yrange = c(min(ymin), max(ymax))
  title  = "Pattern of clusters"
  plot.LAScatalog(ctg, y = FALSE, main = title, xlim = xrange, ylim = yrange)

  #graphics::rect(xmin, ymin, xmax, ymax, border = "red")

  #if (buffer > 0)
    #graphics::rect(xmin - buffer, ymin - buffer, xmax + buffer, ymax + buffer, border = "darkgreen", lty = "dotted")

  lapply(clusters, function(x)
  {
    graphics::rect(x@bbox$xmin, x@bbox$ymin, x@bbox$xmax, x@bbox$ymax, border = "red")

    if (x@buffer > 0)
      graphics::rect(x@bbbox$xmin, x@bbbox$ymin, x@bbbox$xmax, x@bbbox$ymax, border = "darkgreen", lty = "dotted")
  })


  return(clusters)
}