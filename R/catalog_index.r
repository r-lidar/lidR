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
  . <- filename <- Min.X <- Max.X <- Min.Y <- Max.Y <- NULL

  stopifnot(is.list(bboxes))

  queries <- lapply(bboxes, function(bbox)
  {
    bbox  <- bbox + 2*buffer
    is_in <- with(catalog@data, !(Min.X >= bbox@xmax | Max.X <= bbox@xmin | Min.Y >= bbox@ymax | Max.Y <= bbox@ymin))
    files <- catalog@data$filename[is_in]

    if (length(files) == 0 & outside_catalog_is_null)
      return(NULL)
    else if (length(files) == 0 & !outside_catalog_is_null)
      files <- ""

    bbox    <- bbox - 2*buffer
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
