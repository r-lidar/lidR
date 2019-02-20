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

catalog_index =	function(catalog, bboxes, shape = LIDRRECTANGLE, buffer = 0, outside_catalog_is_null = TRUE)
{
  stopifnot(is.list(bboxes))

  MinX <- catalog@data[["Min.X"]]
  MaxX <- catalog@data[["Max.X"]]
  MinY <- catalog@data[["Min.Y"]]
  MaxY <- catalog@data[["Max.Y"]]

  queries <- lapply(bboxes, function(bbox)
  {
    bbbox <- bbox + 2*buffer

    tile_is_in_bbox          <- !(MinX >= bbox@xmax  | MaxX <= bbox@xmin  | MinY >= bbox@ymax  | MaxY <= bbox@ymin)
    tile_is_in_buffered_bbox <- !(MinX >= bbbox@xmax | MaxX <= bbbox@xmin | MinY >= bbbox@ymax | MaxY <= bbbox@ymin)

    if (sum(tile_is_in_bbox) > 0)
      select <- tile_is_in_buffered_bbox
    else
      select <- FALSE

    files  <- catalog@data$filename[select]

    if (length(files) == 0 & outside_catalog_is_null)
      return(NULL)
    else if (length(files) == 0 & !outside_catalog_is_null)
      files <- ""

    center  <- list(x = (bbox@xmax + bbox@xmin)/2, y = (bbox@ymax + bbox@ymin)/2)
    width   <- (bbox@xmax - bbox@xmin)
    height  <- (bbox@ymax - bbox@ymin)
    cluster <- LAScluster(center, width, height, buffer, shape, files, "noname", proj4string = catalog@proj4string)

    cluster@select <- opt_select(catalog)
    cluster@filter <- paste(cluster@filter, opt_filter(catalog))
    return(cluster)
  })

  return(queries)
}
