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



#' Compute metrics for each tree
#'
#' Once the trees are segmented, i.e. attributes exist in the point cloud that reference each
#' tree, computes a set of user-defined descriptive statistics for each individual tree. This is the
#' "tree version" of \link{grid_metrics}.
#'
#' By default the function computes the xyz-coordinates of the highest point of each tree and uses
#' xy as tree coordinates in \code{SpatialPoinsDataFrame}. z is stored in the table of attributes
#' along with the id of each tree. All the other attributes are user-defined attributes:\cr\cr
#' The following existing functions contain a small set of pre-defined metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics_tree}}
#' } Users must write their own functions to create their own metrics. \code{tree_metrics} will
#' dispatch the LiDAR data for each segmented tree in the user-defined function. Functions
#' are defined without the need to consider each segmented tree i.e. only the point cloud (see examples).
#'
#' @template param-las
#' @param func The function to be applied to each tree.
#' @param field character. The column name of the field containing tree IDs. Default is \code{"treeID"}
#'
#' @return A \code{SpatialPoinsDataFrame} that references the xy-position with a table of attributes that
#' associates the z-elevation (highest points) of the trees and the id of the trees, plus the metrics
#' defined by the user.
#'
#' @template LAScatalog
#' @template section-supported-option-tree_detection
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-drop_z_below 0")
#'
#' # Mean height and mean intensity for each tree
#' metrics = tree_metrics(las, list(`Mean Z` = mean(Z), `Mean I` = mean(Intensity)))
#'
#' # Define your own new metrics function
#' myMetrics = function(z, i)
#' {
#'   metrics = list(
#'      imean = mean(i),
#'      imax  = max(i),
#'      npoint = length(z)
#'    )
#'
#'    return(metrics)
#' }
#'
#' metrics = tree_metrics(las, myMetrics(Z, Intensity))
#'
#' # predefined metrics (see ?stdmetrics)
#' metrics = tree_metrics(las, .stdtreemetrics)
#' @export
tree_metrics = function(las, func, field = "treeID")
{
  assert_is_a_string(field)

  UseMethod("tree_metrics", las)
}

#' @export
tree_metrics.LAS = function(las, func, field = "treeID")
{
  . <- X <- Y <- Z <- x.pos.t <- y.pos.t <- NULL

  if (!field %in% names(las@data))
    stop("The trees are not segmented yet. Please see function 'lastrees'.")

  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  call <- lazyeval::as_call(func)

  find_apex <- function(x,y,z)
  {
    j <- which.max(z)
    return(list(x.pos.t = x[j], y.pos.t = y[j], Z = z[j]))
  }

  stats <- las@data[, if (!anyNA(.BY)) c(find_apex(X,Y,Z), eval(call)), by = field]

  if (nrow(stats) == 0)
    stop(glue::glue("The attributes {field} exists but there is no tree segmented. Cannot compute any tree metric."))

  coords <- stats[, .(x.pos.t, y.pos.t)]
  stats[, c("x.pos.t", "y.pos.t") := NULL]

  output <- sp::SpatialPointsDataFrame(coords, stats, proj4string = las@proj4string)
  return(output)
}

#' @export
tree_metrics.LAScluster = function(las, func, field = "treeID")
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  metrics <- tree_metrics(x, func, field)
  bbox    <- raster::extent(las)
  metrics <- raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
tree_metrics.LAScatalog = function(las, func, field = "treeID")
{
  is_formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!is_formula) func <- lazyeval::f_capture(func)
  glob <- future::getGlobalsAndPackages(func)

  options <- list(need_buffer = TRUE, drop_null = TRUE, globals = names(glob$globals))
  output  <- catalog_apply(las, tree_metrics, func = substitute(func), field = field, .options = options)

  if (opt_output_files(las) == "")
  {
    output <- do.call(rbind, output)
    output@proj4string <- las@proj4string
  }
  else
  {
    output <- unlist(output)
  }

  return(output)
}
