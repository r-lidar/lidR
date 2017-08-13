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



#' Apply a user defined function to an entire catalog in a continuous way
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
#' @param ctg A \link[lidR:catalog]{Catalog} object
#' @param func A user's function for which the first input is a LAS object.
#' @param args A list of extra argments to pass in the function 'func'
#' @param ... Any argument avaible in \link{readLAS} to reduce the amount of data loaded.
#' @examples
#' # Visit http://jean-romain.github.io/lidR/wiki for an illustrated and commented
#' # version of this example.
#' # This example is a dummy exemple. It is more efficient to load the entiere file than
#' # splitting it in several pieces to process even using sevral cores.
#'
#' # 1. build a project (here a single file catalog for the need of the example).
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' project = catalog(LASfile)
#' plot(project)
#'
#' # 2. set some global catalog options
#' # For the need of this dummy example the clustering size is 80 m and the buffer is 15 m using
#' # a single core (because this example is ran on CRAN server when package is submitted).
#' catalog_options(buffer = 15, multicore = 1, tiling_size = 80)
#'
#' # 3. load the shapefile you need to filter your points.
#' lake_shp = rgdal::readOGR(folder, "lake_polygons_UTM17")
#'
#' # 4. build the function which analyses each cluster of the catalog.
#' # This function first argument is a LAS object. The internal routine take care of feeding this
#' # function. The other arguments can be freely choosen by the user. See the following template:
#' tree_area = function(las, lake)
#' {
#'   # The las argument is a LAS object with each field loaded and an extra column 'buffer'
#'
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
#'   # Here we used the function tree_metric to compute some metrics for each tree. This
#'   # function is defined later in the global environnement.
#'   m = tree_metrics(las, myMetrics(X, Y, Z, buffer))
#'
#'   # If min buffer is 0 it means the trees were at least parlty in the non buffered area so we
#'   # want to keep this trees.
#'   # In an other hand the trees wich are on the edge of the buffered area will be counted
#'   # twice. So we must remove the tree on the left side and on the bottom side of the buffer
#'   # If max buffer is <= 2 it means that the trees belong inside the area of interest or in
#'   # the left side or in the bottom side or both
#'   m = m[minbuff == 0 & maxbuff <= 2]
#'
#'   # Remove buffering information which are no longer usefull
#'   m[, c("minbuff","maxbuff") := NULL]
#'
#'   return(m)
#' }
#'
#' # This function enable to extract, for a single tree, the position of the highest point and
#' # some informations about the buffing position of the tree. The function tree_metrics take
#' # care of mapping along each tree.
#' myMetrics <- function(x, y, z, buff)
#' {
#'   i = which.max(z)
#'   xcenter = x[i]
#'   ycenter = y[i]
#'   A = lidR:::area(x,y)  # here we used an internal lidR function not exported for users
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
#' # Evering thing is now well defined, let process over an entiere catalog whith
#' # hundred of files (but here a single one for the example...)
#'
#' # 4. Process the project. The arguments of the user's function must
#' # belong in a labelled list. We also pass extra agurment to the function readLAS
#' # to load only X Y and Z coordinates. This way we save a huge amount of memory wich
#' # is useful for the current process.
#' fargs = list(lake = lake_shp)
#' output = catalog_apply(project, tree_area, fargs, XYZonly = TRUE)
#'
#' # 5. Post process the output result (depend on what is the output). Here each values
#' # of the list is a data.table, so rbindlist makes the job
#' output = data.table::rbindlist(output)
#'
#' output %$% plot(x,y, cex = sqrt(area/pi)/5, asp = 1)
#' @export
catalog_apply = function(ctg, func, func_args, ...)
{
  ti      <- Sys.time()
  ncores  <- CATALOGOPTIONS("multicore")
  buffer  <- CATALOGOPTIONS("buffer")
  by_file <- CATALOGOPTIONS("by_file")
  res     <- 1

  # Create a pattern of clusters to be sequentially processed
  ctg_clusters <- catalog_makecluster(ctg, res, buffer, by_file)
  ctg_clusters <- apply(ctg_clusters, 1, as.list)

  output <- catalog_apply_internal(ctg, ctg_clusters, func, func_args, ncores, ...)
  return(output)
}

catalog_apply_internal = function(ctg, clusters, func, func_args, ncores, ...)
{
  if (length(clusters) < ncores)
    ncores = length(clusters)

  ti <- Sys.time()

  # Computations done within sequential or parallel loop in .getMetrics
  if (ncores == 1)
  {
    output = lapply(clusters, .cluster_apply_func, func = func, ctg = ctg, func_args = func_args, ...)
  }
  else
  {
    cat("Begin parallel processing... \n")
    cat("Num. of cores:", ncores, "\n\n")

    varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment()))
    envir   = environment()

    cl = parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl, varlist, envir)
    parallel::clusterEvalQ(cl, library("lidR"))

    output = parallel::parLapply(cl, clusters, fun = .cluster_apply_func, func = func, ctg = ctg, func_args = func_args, ...)
    parallel::stopCluster(cl)
  }

  tf = Sys.time()
  cat("Process done in", round(difftime(tf, ti, units="min"), 1), "min\n\n")

  return(output)
}


.cluster_apply_func = function(ctg_cluster, func, ctg, func_args, ...)
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
  width   = (xright - xleft)/2
  buffer  = ctg_cluster$xrightbuff - ctg_cluster$xright

  # Extract the ROI as a LAS object
  las = catalog_queries_internal(ctg, xcenter, ycenter, width, width, buffer, name, "", 1, FALSE, ...)[[1]]

  # Skip if the ROI fall in a void area
  if (is.null(las)) return(NULL)

  # Because catalog_queries keep point inside the boundingbox (close interval) but point which
  # are exactly on the boundaries are counted twice. Here a post-process to make an open
  # interval on left and bottom edge of the boudingbox.
  if (buffer == 0)
  {
    n = fast_countequal(las@data$X, xleft) + fast_countequal(las@data$Y, ybottom)

    if (n > 0)
      las = suppressWarnings(lasfilter(las, X = xleft, Y > ybottom))

    if (is.null(las))
      return(NULL)
  }

  # Call the function
  return(do.call(func, c(las, func_args)))
}