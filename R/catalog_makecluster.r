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

catalog_makecluster = function(ctg, res, start = c(0,0), plot = TRUE)
{
  xmin    <- ymin <- xmax <- ymax <- 0
  buffer  <- buffer(ctg)
  by_file <- by_file(ctg)
  size    <- tiling_size(ctg)

  # Creation of a set rectangle that encompass the catalog
  # =======================================================

  if (by_file)
  {
    xmin = ctg@data$`Min X`
    xmax = ctg@data$`Max X`
    ymin = ctg@data$`Min Y`
    ymax = ctg@data$`Max Y`
  }
  else
  {
    # Dimension of the clusters (width = height) rounded up to a multiple of the resolution
    width = ceiling(size/res) * res

    # Bounding box of the catalog
    bbox = with(ctg@data, c(min(`Min X`), min(`Min Y`), max(`Max X`), max(`Max Y`)))

    # Shift to align the grid
    shift = numeric(2)
    shift[1] = (bbox[1] - start[1]) %% width
    shift[2] = (bbox[2] - start[2]) %% width

    # Generate coordinates of bottom left clusters corners
    xmin = seq(bbox[1] - shift[1], bbox[3], width)
    ymin = seq(bbox[2] - shift[2], bbox[4], width)
    grid = expand.grid(xmin = xmin, ymin = ymin)
    xmin = grid$xmin
    ymin = grid$ymin
    xmax = xmin + width
    ymax = ymin + width
  }

  verbose("Creating a set of cluster for the catalog...")

  xcenter = (xmin + xmax)/2
  ycenter = (ymin + ymax)/2
  width   = xmax - xmin
  height  = ymax - ymin
  names   = paste0("ROI", 1:length(xcenter))

  # Creation of a set of cluster from the rectangles
  # ================================================

  if (by_file && buffer <= 0)
  {
    clusters = lapply(1:length(xcenter), function(i)
    {
      center = list(x = xcenter[i], y = ycenter[i])
      Cluster(center, width[i], height[i], buffer, LIDRRECTANGLE, ctg@data$filename[i], names[i])
    })
  }
  else
  {
    clusters = suppressWarnings(catalog_index(ctg, xcenter, ycenter, width, height, buffer, names))
  }

  # Post process the clusters
  # =========================

  # Specific case for computation speed
  # -----------------------------------

  if (by_file & buffer == 0)
  {
    clusters <- lapply(clusters, function(x)
    {
      x@filter <- ""
      return(x)
    })
  }

  # Record the path to write the raster if requested
  # ------------------------------------------------

  if (save_vrt(ctg))
  {
    clusters <- lapply(clusters, function(x)
    {
      x@save <- paste0(vrt(ctg), "/tile-", x@bbox$xmin, "-", x@bbox$ymin, ".tiff")
      return(x)
    })
  }

  # Plot the catalog and the clusters
  # =================================

  if(plot)
  {
    xrange = c(min(xmin), max(xmax))
    yrange = c(min(ymin), max(ymax))
    title  = "Pattern of clusters"
    plot.LAScatalog(ctg, y = FALSE, main = title, xlim = xrange, ylim = yrange)

    lapply(clusters, function(x)
    {
      graphics::rect(x@bbox$xmin, x@bbox$ymin, x@bbox$xmax, x@bbox$ymax, border = "red")

      if (x@buffer != 0)
        graphics::rect(x@bbbox$xmin, x@bbbox$ymin, x@bbbox$xmax, x@bbbox$ymax, border = "darkgreen", lty = "dotted")
    })
  }

  return(clusters)
}