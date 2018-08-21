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



#' Apply a user-defined function to an entire catalog in a continuous way
#'
#' This function enables application of a user-defined routine over an entire catalog using a
#' multi-core process. When a user has a dataset organized into several files, it applies the
#' user-defined function to the entire catalog by automatically splitting it into several
#' clusters. The clustering pattern can be either split into a set of squared areas or split by
#' file. The clustering pattern can be modified using the catalog options (see \link{catalog}.
#' The "Examples" section describes the procedure for applying functions to the catalog, beginning
#' with data loading (see example). \cr\cr
#' \strong{Warning:} there is a mechanism to load buffered data and to avoid edge artifacts,
#' but no mechanism to remove the buffer after applying user-defined functions, since this task
#' is very specific to each process. See section "Edge artifacts".\cr\cr
#' \code{lidR} supports .lax files. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @section Edge artifacts:
#'
#' It is very important to take precautions to avoid 'edge artifacts' when processing LiDAR
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process. The lidR package provides internal tools to load buffered
#' data. However, there is no mechanism to remove the results computed in the buffered area
#' since this task depends on the output of the user-defined function. Therefore, depending
#' on the metric being computed, some output results could appear several times.\cr\cr
#' The LAS object received by the user-defined function has a special column called 'buffer_side'
#' which indicates, for each point, if it comes from a buffered area or not. Points
#' from non-buffered areas have a 'buffer_side' value of zero, while points from buffered areas
#' have a 'buffer_side' value of 1, 2, 3 or 4, where 1 is the bottom buffer and 2, 3 and 4 are the
#' left, top and right buffers, respectively (see example).
#'
#' @aliases catalog_apply
#' @param ctg A \link[lidR:catalog]{LAScatalog} object.
#' @param func A user-defined function for which the first input is a LAS object.
#' @param func_args A list of extra arguments to pass in the function 'func'.
#' @param ... Any argument available in \link{readLAS} to reduce the amount of data loaded.
#' @examples
#' \dontrun{
#' # Visit http://jean-romain.github.io/lidR/wiki for an illustrated and commented
#' # version of this example.
#' # This is a dummy example. It is more efficient to load the entire file than
#' # splitting it into several pieces to process, even when using multiple cores.
#'
#' # 1. Build a project (here, a single file catalog for the purposes of this example).
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' project = catalog(LASfile)
#' plot(project)
#'
#' # 2. Set some catalog options
#' # For this dummy example, the clustering size is 80 m and the buffer is 15 m using
#' # a single core (because this example is run on the CRAN server when the package is submitted).
#' buffer(project) = 15
#' cores(project) = 1
#' tiling_size(project) = 120
#'
#' # 3. Load the shapefile needed to filter your points.
#' folder <- system.file("extdata", "", package="lidR")
#' lake_shp = rgdal::readOGR(folder, "lake_polygons_UTM17")
#'
#' # 4. Build the function that analyzes each cluster of the catalog.
#' # The function's first argument is a LAS object. The internal routine takes care of
#' # this part. The other arguments can be freely choosen by the user. See the following
#' # template:
#' tree_area = function(las, lake)
#' {
#'   # The las argument is a LAS object with each field loaded and an extra column 'buffer'
#'
#'   # Associate geographic data with lidar points
#'   lasclassify(las, lake, field = "lake")
#'
#'   # filter lakes, and low elevation points
#'   las = lasfilter(las, lake == FALSE, Z > 4)
#'
#'   if (is.null(las))
#'     return(NULL)
#'
#'   # segment trees (in this example the low point density does not enable
#'   # accurate segmentation of trees. This is just a proof-of-concept)
#'   chm = grid_canopy(las, 1, subcircle = 0.3)
#'   chm = as.raster(chm)
#'   kernel = matrix(1,3,3)
#'   chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'   lastrees(las, algorithm = "watershed", chm = chm)
#'
#'   # Here we used the function tree_metric to compute some metrics for each tree. This
#'   # function is defined later in the global environment.
#'   m = tree_metrics(las, myMetrics(X, Y, Z, buffer))
#'
#'   # If min buffer is 0 it means the trees were at least partly in the non-buffered area, so we
#'   # want to keep these trees.
#'   # However, the trees that are on the edge of the buffered area will be counted
#'   # twice. So we must remove the trees on the right side and on the top side of the buffer
#'   # If max buffer is <= 2 it means that the trees belong inside the area of interest, on
#'   # the left side or the bottom side, or both.
#'   m = m[minbuff == 0 & maxbuff <= 2]
#'
#'   # Remove buffering information that is no longer useful
#'   m[, c("minbuff","maxbuff") := NULL]
#'
#'   return(m)
#' }
#'
#' # This function enables users to extract, for a single tree, the position of the highest point
#' # and some information about the buffering position of the tree. The function tree_metrics takes
#' # care of mapping along each tree.
#' myMetrics <- function(x, y, z, buff)
#' {
#'   i = which.max(z)
#'   xcenter = x[i]
#'   ycenter = y[i]
#'   A = area(x,y)
#'   minbuff = min(buff)
#'   maxbuff = max(buff)
#'
#'   return(
#'     list(
#'       x = xcenter,
#'       y = ycenter,
#'       area = A,
#'       minbuff = minbuff,
#'       maxbuff = maxbuff
#'     ))
#' }
#'
#' # Everything is now well defined, so now we can process over an entire catalog with
#' # hundreds of files (but in this example we use just one file...)
#'
#' # 4. Process the project. The arguments of the user-defined function must
#' # belong in a labelled list. We also pass extra arguments to the function readLAS
#' # to load only X, Y and Z coordinates. This way we save a huge amount of memory, which
#' # can be used for the current process.
#' fargs = list(lake = lake_shp)
#' output = catalog_apply(project, tree_area, fargs, select = "xyz")
#'
#' # 5. Post-process the output result (depending on the output computed). Here, each value
#' # of the list is a data.table, so rbindlist does the job:
#' output = data.table::rbindlist(output)
#'
#' with(output, plot(x,y, cex = sqrt(area/pi)/5, asp = 1))
#' }
#' @export
catalog_apply <- function(ctg, func, func_args = NULL, ...)
{
  assertive::assert_is_all_of(ctg, "LAScatalog")
  assertive::assert_is_function(func)

  progress  <- progress(ctg)
  ncores    <- cores(ctg)
  stopearly <- stop_early(ctg)
  clusters  <- catalog_makecluster(ctg, 1)
  output    <- cluster_apply(clusters, func, ncores, progress, stopearly, func_args = func_args, ..., autoread = TRUE)
  return(output)
}