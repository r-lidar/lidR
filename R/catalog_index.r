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



# Retrieve the tiles containing ROIs
#
# When the user has a set of (x, y) coordinates corresponding to a region of interest (ROI)
# (a ground inventory, for example), they can automatically find the tiles containing the lidar data associated
# with the ROIs from a \link[lidR:catalog]{LAScatalog}. The algorithm will do this even for ROIs falling on the edges of one or
# more tiles.\cr
# It only works for tiles that are arranged in gridlines. This function is used by \link[lidR:catalog_queries]{catalog_queries}.
# Users do not really need it.
#
# @aliases catalog_index
# @param catalog A LAScatalog object
# @param x vector. A set of x plot coordinates
# @param y vector. A set of y plot coordinates
# @param r numeric or vector. A radius or a set of radii of the ROI. If only
# r is provided (r2 = NULL) it will extract data falling onto a disc.
# @param r2 numeric or vector. A radius or a set of radii of plots. If r2
# is provided, the selection turns into a rectangular ROI. If r= r2 it is a square.
# @param roinames vector. A set of ROI names
# @seealso
# \link[lidR:catalog]{LAScatalog}
# \link[lidR:catalog_queries]{catalog_queries}
catalog_index =	function(catalog, x, y, r, r2, buffer, roinames)
{
  tile <- minx <- maxx <- miny <- maxy <- NULL
  filename <- `Min X` <- `Max X` <- `Min Y` <- `Max Y` <- NULL
  . <- NULL

  nplot <- length(x)
  shape <- LIDRRECTANGLE

  if(is.null(r2))
  {
    r2    <- r
    shape <- LIDRCIRCLE
  }

  if (buffer > 0)
  {
    r  <- r + buffer
    r2 <- r2 + buffer
  }

  coord.tiles <- catalog@data[, .(filename, `Min X`, `Max X`, `Min Y`, `Max Y`)]
  data.table::setnames(coord.tiles, c("tile", "minx", "maxx", "miny", "maxy"))

  coord.plot <- data.table(roinames, x, y, r, r2)
  coord.plot[,`:=`(maxx = x + r, maxy = y + r2, minx = x - r, miny = y - r2)]

  tiles <- lapply(1:nplot, function(i)
  {
    coord <- coord.plot[i]
    coord.tiles[!(minx >= coord$maxx | maxx <= coord$minx | miny >= coord$maxy | maxy <= coord$miny)]$tile
  })

  coord.plot[, `:=`(buffer = buffer, shape = shape)]
  coord.plot[, tiles := list(tiles)]
  coord.plot[, c("maxx", "maxy", "minx", "miny") := NULL][]

  # Check if some files were outside the catalog
  numfile <- coord.plot$tiles %>% sapply(length)
  n <- numfile == 0

  if(sum(n) > 0)
  {
    for (roi in coord.plot$roinames[n])
    {
      msg = paste(roi, "is outside the catalog.")
      warning(msg, call. = FALSE)
    }
  }

  keep = !n
  coord.plot = coord.plot[keep]
  roinames = roinames[keep]

  queries = apply(coord.plot, 1, as.list)
  names(queries) = roinames

  return(queries)
}