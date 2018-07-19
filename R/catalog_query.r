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


#' Extract LiDAR data based on a set of coordinates
#'
#' From a set of (x, y) coordinates corresponding to the centers of regions of interest (ROIs),
#' for example a ground inventory, the function automatically extracts the lidar data associated
#' with the ROIs from a \link{catalog}. The algorithm will do this even for ROIs falling on
#' the edges of one or more tiles. The extracted lidar data can be buffered. In this case the
#' function adds a buffer area around the ROIs, and the LAS object returned has an extra column
#' named '\code{buffer}' which indicates, for each point, if the point is in the buffer or
#' from the ROI (see more in the section Buffer).\cr\cr
#' \code{lidR} support .lax file. You will speed-up the computation \emph{a lot} with a spatial
#' index.
#'
#' @section Buffer:
#' If the ROIs are buffered then the LAS objects returned by the function have extra points.
#' The LAS objects received by the user contain a special column called 'buffer', which indicates,
#' for each point, if it comes from a buffered area or not. Points from non-buffered areas (i.e.
#' the ROI) have a 'buffer' value of 0, while those from buffered areas have a 'buffer' value
#' greater than 0.\cr\cr
#' For a circular ROI, points in the buffered area have a buffer value of 1. For a rectangular
#' ROI the points in the buffer area have a buffer value of 1, 2, 3 or 4, where 1 is the bottom
#' buffer and 2, 3 and 4 are the left, top and right buffers, respectively.
#'
#' @section Multicore computation:
#' The process is done using several cores. To change the settings of how a catalog is processed
#' use \link{cores}.
#'
#' @param ctg A LAScatalog object
#' @param x vector. A set of x coordinates corresponding to the centers of the ROIs
#' @param y vector. A set of y coordinates corresponding to the centers of the ROIs
#' @param r numeric or vector. A radius or a set of radii of the ROIs. If only
#' r is provided (r2 = NULL) it will extract data falling onto a disc.
#' @param r2 numeric or vector. A radius or a set of radii of plots. If r2
#' is provided, the selection turns into a rectangular ROI (if r = r2 it is a square).
#' @param buffer numeric. Adds a buffer area around the ROI. See relevant sections.
#' @param roinames vector. A set of ROI names (the plot IDs, for example) to label the
#' returned list.
#' @param ... Any argument available in \link{readLAS} to reduce the amount of data loaded.
#' @return A list of LAS objects
#' @seealso
#' \link{readLAS}
#' \link{catalog}
#' @export
#' @examples
#' \dontrun{
#' # Build a LAScatalog
#' catalog = catalog("<Path to a folder containing a set of .las or .laz files>")
#'
#' # Get coordinates from an external file
#' X = runif(30, 690000, 800000)
#' Y = runif(30, 5010000, 5020000)
#' R = 25
#'
#' # Return a List of 30 circular LAS objects of 25 m radius
#' catalog_queries(catalog, X, Y, R)
#'
#' # Return a List of 30 square LAS objects of 50x50 m
#' catalog_queries(catalog, X, Y, R, R)
#'
#' # Return a List of 30 circular LAS objects of 30 m radius. 25 m being the ROI and 5 m
#' # being a buffered area. The LAS objects have an extra column called 'buffer' to
#' # differentiate the points.
#' catalog_queries(catalog, X, Y, R, buffer = 5)
#'
#' # Return a List of 30 circular LAS objects of 25 m radius for which only the fields X, Y and
#' # Z have been loaded and Z values < 0 were removed.
#' catalog_queries(catalog, X, Y, R, select = "xyz", filter = "-drop_z_below 0")
#' }
catalog_queries = function(ctg, x, y, r, r2 = NULL, buffer = 0, roinames = NULL, ...)
{
  UseMethod("catalog_queries", ctg)
}

#' @export
catalog_queries.LAScatalog = function(ctg, x, y, r, r2 = NULL, buffer = 0, roinames = NULL, ...)
{
  assertive::assert_are_same_length(x, y)
  assertive::assert_all_are_non_negative(buffer)
  if (length(r) > 1) assertive::assert_are_same_length(x, r)
  if (length(buffer) > 1) assertive::assert_are_same_length(x, buffer)
  if (is.null(roinames)) roinames <- paste0("ROI", 1:length(x))

  progress  <- progress(ctg)
  ncores    <- cores(ctg)
  stopearly <- stop_early(ctg)

  w <- 2*r
  h <- if(is.null(r2)) NULL else 2*r2

  if (progress)
    plot.LAScatalog(ctg, FALSE)

  clusters <- catalog_index(ctg, x, y, w, h, buffer, roinames)
  output   <- cluster_apply(clusters, readLAS, ncores, progress, stopearly, ...)
  names(output) <- names(clusters)
  
  # Transfer CRS
  for (i in 1:length(output)){
    output[[i]]@crs <- ctg@crs
  }

  # Patch to solves issue #73 waiting for a better solution in issue 2333 in data.table
  if (ncores > 1)
  {
    for (i in 1:length(output))
      output[[i]]@data <- data.table::alloc.col(output[[i]]@data)
  }

  return(output)
}
