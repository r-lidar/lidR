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



#' Retrieve the files containing ROIs
#'
#' \bold{This function is an internal function reserved for lidR developers.\cr\cr}
#' When we have a set of (x, y) coordinates corresponding to a region of interest (ROI) in a catalog
#' this function retrieve in which file(s) are these ROIs.
#'
#' @param catalog A LAScatalog object
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param r numeric or vector. A radius or a set of radii of the ROI. If only
#' r is provided (r2 = NULL) it will extract data falling onto a disc.
#' @param r2 numeric or vector. A radius or a set of radii of plots. If r2
#' is provided, the selection turns into a rectangular ROI. If r= r2 it is a square.
#' @param roinames vector. A set of ROI names
#' @return A list of LAScluster object.
#' @keywords internal
catalog_index =	function(catalog, x, y, w, h, buffer, roinames)
{
  tile <- minx <- maxx <- miny <- maxy <- NULL
  filename <- `Min X` <- `Max X` <- `Min Y` <- `Max Y` <- NULL
  . <- NULL

  nplot  <- length(x)
  shape  <- LIDRRECTANGLE
  width  <- w
  height <- h

  if(is.null(h))
  {
    h  <- w
    height <- h
    shape <- LIDRCIRCLE
  }

  if (buffer > 0)
  {
    w <- w + buffer
    h <- h + buffer
  }

  coord.tiles <- catalog@data[, .(filename, `Min X`, `Max X`, `Min Y`, `Max Y`)]
  data.table::setnames(coord.tiles, c("tile", "minx", "maxx", "miny", "maxy"))

  coord.plot <- data.table::data.table(roinames, x, y, w, h)
  coord.plot[,`:=`(maxx = x + w, maxy = y + h, minx = x - w, miny = y - h)]

  tiles <- lapply(1:nplot, function(i)
  {
    coord <- coord.plot[i]
    coord.tiles[!(minx >= coord$maxx | maxx <= coord$minx | miny >= coord$maxy | maxy <= coord$miny)]$tile
  })

  coord.plot[, tiles := list(tiles)]

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

  if (!any(keep))
    return(NULL)

  coord.plot = coord.plot[keep]
  roinames = roinames[keep]

  queries = apply(coord.plot, 1, function(cl)
  {
    center = list(x = cl$x, y = cl$y)
    Cluster(center, cl$w-buffer, cl$h-buffer, buffer, shape, cl$tiles, cl$roinames)
  })

  names(queries) = roinames

  return(queries)
}