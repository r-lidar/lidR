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



#' Apply functions over a LAScatalog
#'
#' This function gives access at the user level to the \link[lidR:LAScatalog-class]{LAScatalog} processing
#' engine. It allows the application of a user-defined routine over an entire catalog using a
#' multi-core process (see detail).
#'
#' The LAScatalog processing engine tools is explained in the \link[lidR:LAScatalog-class]{LAScatalog class}
#' \strong{Warning:} the LAScatalog processing engine have a mechanism to load buffered data to avoid
#' edge artifacts, but no mechanism to remove the buffer after applying user-defined functions, since
#' this task is very specific to each process. In other \code{lidR} functions this task is performed
#' specifically for each function. In \code{catalog_apply} the users's function can return any input,
#' thus the user must take care of this point himself (See section "Edge artifacts")
#'
#' @template LAScatalog
#'
#' @section Edge artifacts:
#'
#' It is very important to take precautions to avoid 'edge artifacts' when processing LiDAR
#' tiles. If the points from neighboring tiles are not included during certain processes,
#' this could create 'edge artifacts' at the tile boundaries. For example, empty or incomplete
#' pixels in a rasterization process or dummy elavations in a ground interpolation.
#' The LAScatalog processing engine provides internal tools to load buffered data. However, there is
#' no mechanism to remove the results computed in the buffered area since this task depends on the
#' output of the user-defined function.
#'
#' @section Buffered data:
#'
#' The LAS objects read by the user function have a special column called 'buffer'
#' which indicates, for each point, if it comes from a buffered area or not. Points
#' from non-buffered areas have a 'buffer' value of 0, while points from buffered areas
#' have a 'buffer' value of 1, 2, 3 or 4, where 1 is the bottom buffer and 2, 3 and 4 are the
#' left, top and right buffers, respectively
#'
#' @section Function template:
#'
#' The parameter \code{FUN} expect a function that have a first argument that will be fed automatically
#' by the \code{LAScatalog} processing engine. This first argument is a \code{LAScluster}. A
#' \code{LAScluster} is an internal undocumented class but the user needs to know only two this about
#' this class:
#' \itemize{
#' \item The function \link{readLAS} can be used with a \code{LAScluster}
#' \item The function \link[raster:extent]{extent} or \link[sp:bbox]{bbox} can be used with a \code{LAScluster}
#' and it returns the bouding box of the cluster whitout the buffer. It can be used to clip the ouput and
#' remove the buffered region (see examples)
#' }
#' A user-defined function must be templated like this:
#' \preformatted{
#' myfun = function(cluster, ....)
#' {
#'    las = readLAS(cluster)
#'    if (is.empty(las)) return(NULL)
#'    # do something
#'    return(something)
#' }}
#' The line \code{if(is.empty(las)) return(NULL)} is of major importance because some cluster may contain
#' 0 point (we can't know that before to read the file). In this case an empty point cloud with 0 point
#' is returned by \code{readLAS} and this may fails in subsequent code. Thus, exiting early the user-function
#' by returning \code{NULL} allows the internal engine to knows that the cluster was empty.
#'
#' @template section-supported-option-catalog_apply
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object.
#' @param FUN A user-defined function that respect a given template (see section function template)
#' @param ... Optional arguments to FUN.
#'
#' @examples
#' # Visit http://jean-romain.github.io/lidR/wiki for more examples
#'
#' ## =========================================================================
#' ## Exemple 1: get all the tree tops over an entiere catalog
#' ## (this is nothing else that the existing lidR function tree_detection)
#' ## =========================================================================
#'
#' # 1. Build the user-defined function that analyzes each cluster of the catalog.
#' # The function's first argument is a LAScluster object. The other arguments can be freely
#' # choosen by the user.
#' my_tree_detection_method = function(cluster, ws)
#' {
#'   # The cluster argument is a LAScluster object. The user don't need to know how it works.
#'   # readLAS will load the region of interest with a buffer around it, taking advantage of
#'   # point cloud indexation if possible. The filter and select options are propagated automatically
#'   las = readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'
#'   # Find the tree tops using a user-developped method for example (here simply a LMF)
#'   ttops = tree_detection(las, lmf(ws))
#'
#'   # ttops is a SpatialPointsDataFrame that contains the tree tops in our region of interest
#'   # plus the trees tops in the buffered area. We need to remove buffer otherwise we will get
#'   # some trees more than once.
#'   bbox  <- raster::extent(cluster)
#'   ttops <- raster::crop(ttops, bbox)
#'
#'   return(ttops)
#' }
#'
#' # 2. Build a project (here, a single file catalog for the purposes of this dummmy example).
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' project <- catalog(LASfile)
#' plot(project)
#'
#' # 3. Set some catalog options
#' # For this dummy example, the clustering size is 80 m and the buffer is 10 m using a single core.
#' set_buffer(project) <- 10
#' set_cores(project) <- 1L
#' set_tiling_size(project) <- 80       # extremely tiny because this is a dummy example
#' set_select(project) <- "xyz"         # don't need to read something else than the coordinates
#' set_filter(project) <- "-keep_first" # for this exemple we will use only first returns. why not
#'
#' # 4. Apply user-defined function to take advantage of the internal engine
#' output = catalog_apply(project, my_tree_detection_method, ws = 5)
#'
#' # 5. Post-process the output to merge the results (depending on the output computed).
#' # Here, each value of the list is a SpatialPointsDataFrame, so rbind does the job:
#' output = do.call(rbind, output)
#' sp::plot(output)
#'
#' ## ===================================================
#' ## Exemple 2: compute a rumple index on surface points
#' ## ===================================================
#'
#' rumple_index_surface = function(cluster, res)
#' {
#'   las = readLAS(cluster)
#'   if (is.empty(las)) return(NULL)
#'
#'   las = lasfiltersurfacepoints(las, 0.5)
#'
#'   rumple <- grid_metrics(las, rumple_index(X,Y,Z), res)
#'   bbox   <- raster::extent(cluster)
#'   rumple <- raster::crop(rumple, bbox)
#'
#'   return(rumple)
#' }
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' ?cataproject <- catalog(LASfile)
#'
#' set_buffer(project) <- 1
#' set_cores(project) <- 1L
#' set_tiling_size(project) <- 80       # extremely tiny because this is a dummy example
#' set_select(project) <- "xyz"         # don't need to read something else than the coordinates
#'
#' output = catalog_apply(project, rumple_index_surface, res = 20)
#' output = do.call(raster::merge, output)
#' plot(output, col = height.colors(50))
#' @export
catalog_apply <- function(ctg, FUN, ...)
{
  assertive::assert_is_all_of(ctg, "LAScatalog")
  assertive::assert_is_function(FUN)
  output <- catalog_apply2(ctg, FUN, ..., need_buffer = FALSE, check_alignement = FALSE, drop_null = TRUE, need_output_file = FALSE)
  return(output)
}

catalog_apply2 =  function(ctg, FUN, ..., need_buffer = FALSE, check_alignement = FALSE, drop_null = FALSE, need_output_file = FALSE)
{
  assertive::assert_is_function(FUN)
  assertive::assert_is_a_bool(need_buffer)
  assertive::assert_is_a_bool(check_alignement)
  assertive::assert_is_a_bool(drop_null)

  p        <- list(...)
  res      <- if (is.null(p$res)) 0 else p$res
  start    <- if (is.null(p$start)) c(0,0) else p$start
  ctg      <- .catalog_apply_check_and_fix_options(ctg, need_buffer, check_alignement, need_output_file, res = res, start = start)
  clusters <- catalog_makecluster(ctg)
  clusters <- .catalog_apply_check_and_fix_clusters(ctg, clusters, check_alignement, res = res, start = start)
  output   <- cluster_apply(clusters, FUN, processing_options = ctg@processing_options, output_options = ctg@output_options, drop_null = drop_null, ...)

  return(output)
}

.catalog_apply_check_and_fix_options = function(ctg, need_buffer, check_alignement, need_output_file, res = NULL, start = NULL)
{
  if (need_buffer & get_buffer(ctg) <= 0)
    stop("A buffer greater than 0 is requiered to process the catalog. See help(\"LAScatalog-class\", \"lidR\")", call. = FALSE)

  # If we want to return a Raster*, to ensure a strict continuous output we need to check if the
  # clusters are aligned with the pixels. In case of tiling_size > 0 it is easy to check before to make
  # the clusters
  if (check_alignement & !get_by_file(ctg))
  {
    # If the clustering option do not match with the resolution
    t_size     <- get_tiling_size(ctg)
    new_t_size <- round_any(t_size, res)
    if (new_t_size != t_size)
    {
      set_tiling_size(ctg) <- new_t_size
      message(glue::glue("Clustering size do no match with the resolution of the Raster. Clustering size changed to {new_t_size} to ensure the continuity of the ouput."))
    }

    # If the alignement of the clusters do not match with the start point of the raster
    alignment     <- get_alignment(ctg)
    new_alignment <- (alignment - start) %% res + alignment
    if (any(new_alignment != alignment))
    {
      set_alignment(ctg) <- new_alignment
      message(glue::glue("Alignement of the clusters do no match with the starting points of the RasterLayer. Alignment changed to ({new_alignment[1]}, {new_alignment[2]}) to ensure the continuity of the ouput."))
    }
  }

  # Some functions require to write outputs in files because the output it likely to be to  big to
  # be returned in R
  if (need_output_file & get_output_files(ctg) == "")
    stop("This function requieres that the LAScatalog provides an output file template. See  help(\"LAScatalog-class\", \"lidR\")", call. = FALSE)

  return(ctg)
}

.catalog_apply_check_and_fix_clusters = function(ctg, clusters, check_alignement, res = NULL, start = NULL)
{
  if (check_alignement & get_by_file(ctg))
  {
    for(i in 1:length(clusters))
    {
      cluster <- clusters[[i]]
      bbox1 <- raster::extent(cluster)
      bbox2 <- bbox1
      xmin <- round_any(bbox1@xmin, res)
      ymin <- round_any(bbox1@ymin, res)
      xmax <- round_any(bbox1@xmax, res)
      ymax <- round_any(bbox1@ymax, res)
      bbox2@xmin <- if (xmin >= bbox1@xmin) xmin - res else xmin
      bbox2@ymin <- if (ymin >= bbox1@ymin) ymin - res else ymin
      bbox2@xmax <- if (xmax <= bbox1@xmax) xmax + res else xmax
      bbox2@ymax <- if (ymax <= bbox1@ymax) ymax + res else ymax

      if (!bbox1 == bbox2)
      {
        new_cluster <- catalog_index(ctg, list(bbox2), LIDRRECTANGLE, get_buffer(ctg))[[1]]
        clusters[[i]] <- new_cluster
        message(glue::glue("Cluster {i} has been slighly extended compared to the original file to ensure the continuity of the ouput."))
      }
    }
  }

  return(clusters)
}
