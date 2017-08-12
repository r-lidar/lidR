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
#' Once the trees are segmented with \link{lastrees}, computes a series of descriptive statistics
#' defined by the user for each individual tree. The output is a table in which each line is a tree,
#' and each column is a metric. \code{tree_metrics} is similar to \link{lasmetrics} or \link{grid_metrics}
#' or \link{grid_metrics3d} or \link{grid_hexametrics}, except it computes metrics for each segmented tree.
#'
#' The following existing functions contain a small set of pre-defined metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics_tree}}
#' } Users must write their own functions to create their own metrics. \code{tree_metrics} will
#' dispatch the LiDAR data for each segmented tree in the user-defined function. Functions
#' are defined without the need to considering each segmented tree i.e. only the point cloud (see examples).
#'
#' @param .las An object of class \code{LAS}.
#' @param func The function to be applied to each tree.
#' @param debug logical. When facing a non trivial error, try \code{debug = TRUE}.
#' @return Returns a \code{data.table} containing the metrics for each segmented tree.
#' @examples
#' LASfile <- system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-drop_z_below 0")
#'
#' # segment trees (see lastrees)
#' lastrees(las, algorithm = "li2012")
#'
#' # Max height for each tree
#' tree_metrics(las, mean(Z))
#'
#' # Define your own new metrics
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
tree_metrics = function(.las, func, debug = FALSE)
{
  UseMethod("tree_metrics", .las)
}

#' @export
tree_metrics.LAS = function(.las, func, debug = FALSE)
{
  call = substitute(func)

  stat <- lasaggregate(.las, by = "TREE", call, NA, NA, c("tree"), FALSE, debug)

  return(stat)
}
