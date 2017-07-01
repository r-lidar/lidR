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



#' Apply a user defined function to an entire catalog
#'
#' This function enable to apply a user defined routine over an entiere catalog using a multi-core
#' process. When a user has a dataset organized into several files, it applies a user defined
#' function to the entiere catalog by automatically splitting the catalog into several clusters.
#' The clustering pattern can be either split as a set of squared areas or can be split by file.
#' The clustering pattern can be modified using the global catalog options with \link{catalog_options}.
#' The "Examples" section describes the procedure to apply functions to the catalog, beginning
#' with data loading (see example). \cr\cr
#' \strong{Warning:} there is a buffer mechanism to load buffered data and to avoid edge artifacts
#' but no mechnism to remove the buffer after applying the user defined functions since this task.
#' is very specific to each process. See section "Edge artifacts".
#'
#' @section Edge artifacts:
#'
#' It is very important to take precautions to avoid 'edge artifacts' when processing LiDAR
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the edges of the tiles. For example, empty or incomplete
#' pixels in a rasterization process. The lidR package provides internal tools to load buffered
#' data. However there is not mechanism to remove the results computed in the buffered area
#' since this task depends on the output of the user defined function. Therefore, depending
#' on what the user is computing some outputs results can appear several time.\cr\cr
#' The LAS object received by the user defined function has a special colum called 'buffer_side'
#' which enable, for each point, to know if the point comes from a buffered areas or not. Points
#' from not buffered areas have a 'buffer_side' of zero while points from buffered areas have
#' a 'buffer_side' of 1, 2, 3 or 4. 1 being the bottom buffer and 2, 3 and 4 being respectively
#' the left, top and right buffer (see example).
#'
#' @aliases catalog_apply
#' @param x  A \link[lidR:catalog]{Catalog} object
#' @param func A function which has one parameter: the name of a .las or .laz file (see example)
#' @examples
#' # Visit http://jean-romain.github.io/lidR/wiki for more examples
#' # about this function
#'
#' # 1. build a project (here a single tiny file catalog for the need of example)
#' folder <- system.file("extdata", "", package="lidR")
#' project = catalog(folder)
#' project = project[1,]
#' plot(project)
#'
#' # 2. set some global catalog options
#' # For the need of this dummy example the clustering size is 80 m and the buffer is 10 m using
#' # a single core.
#' catalog_options(by_file = FALSE, buffer = 10, multicore = 1, tiling_size = 80)
#'
#' # 3. load the shapefile you need to filter your points.
#' lake_shp = rgdal::readOGR(folder, "lake_polygons_UTM17")
#'
#' # 4. build the function which analyses a cluster of the catalog.
#' # This function first argument is a LAS object. See the following template:
#' myAnalyse = function(las, lake)
#' {
#'   # Associate geographic data with lidar points
#'   lasclassify(las, lake, field = "lake")
#'
#'   # filter lakes, and low elevation points
#'   las %<>% lasfilter(lake == FALSE, Z > 4)
#'
#'   # segment trees (in this example the low point density does not enable
#'   # to segment tree properly. This is just a proof of concept)
#'   lastrees(las, algorithm = "li2012")
#'
#'   # count the trees
#'   num_tree = length(unique(las@data$treeID))
#'
#'   return(num_tree)
#' }
#'
#' # 4. Process the project.
#' output = catalog_apply(project, myAnalyse, lake = lake_shp)
#'
#' # 5. Post process the output results (depend of what is the output)
#' output = unlist(output)
#' total_trees = sum(output)
#' @export
catalog_apply = function(ctg, func, ...)
{
  ti = Sys.time()

  numcores = CATALOGOPTIONS("multicore")
  buffer   = CATALOGOPTIONS("buffer")
  by_file  = CATALOGOPTIONS("by_file")
  res      = 1

  # Create a pattern of clusters to be sequentially processed
  ctg_clusters = catalog_makecluster(ctg, res, buffer, by_file)
  ctg_clusters = apply(ctg_clusters, 1, as.list)

  # Computations done within sequential or parallel loop in .getMetrics
  if (numcores == 1)
  {
    output = lapply(ctg_clusters, apply_func, func = func, ctg = ctg, ...)
  }
  else
  {
    cat("Begin parallel processing... \n")
    cat("Num. of cores:", numcores, "\n\n")

    cl = parallel::makeCluster(numcores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, ctg_clusters, fun = apply_func, func = func, ctg = ctg, ...)
    parallel::stopCluster(cl)
  }

  # cl <- parallel::makeCluster(getOption("cl.cores", mc.cores))
  # parallel::clusterExport(cl, varlist, envir = environment())
  # out = parallel::parLapplyLB(cl, files, func)
  # parallel::stopCluster(cl)


  tf = Sys.time()
  cat("Process done in", round(difftime(tf, ti, units="min"), 1), "min\n\n")

  return(output)
}


apply_func = function(ctg_cluster, func, ctg, ...)
{
  X <- Y <- NULL

  # Variables for readability
  xleft   = ctg_cluster$xleft
  xright  = ctg_cluster$xright
  ybottom = ctg_cluster$ybottom
  ytop    = ctg_cluster$ytop
  name    = "ROI" %+% ctg_cluster$name
  path    = ctg_cluster$path
  xcenter = ctg_cluster$xcenter
  ycenter = ctg_cluster$ycenter
  width   = (ctg_cluster$xrightbuff - ctg_cluster$xleftbuff)/2

  # Extract the ROI as a LAS object
  las = catalog_queries(ctg, xcenter, ycenter, width, width, name, filter = " ", disable_bar = T, no_multicore = T)[[1]]

  # Skip if the ROI fall in a void area
  if (is.null(las)) return(NULL)

  # Because catalog_queries keep point inside the boundingbox (close interval) but point which
  # are exactly on the boundaries are counted twice. Here a post-process to make an open
  # interval on left and bottom edge of the boudingbox.
  las = suppressWarnings(lasfilter(las, X > xleft, Y > ybottom))

  # Very unprobable but who knows...
  if (is.null(las)) return(NULL)

  # Add an information about buffering
  las@data[, buffer_side := 0]
  las@data[Y < ybottom, buffer_side := 1]
  las@data[X < xleft,   buffer_side := 2]
  las@data[Y > ytop,    buffer_side := 3]
  las@data[X > xright,  buffer_side := 4]

  # Call the function
  return(func(las, ...))
}