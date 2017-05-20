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
#' defined by the user for each individual tree. Output is a table in which each line is a tree,
#' and each column is a metric. \code{tree_metrics} is similar to \link{lasmetrics} or \link{grid_metrics}
#' or \link{grid_metrics3d} or \link{grid_hexametrics} except it computes metrics for each tree.
#'
#' The following existing functions can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:stdmetrics]{stdmetrics_tree}}
#' } Users must write their own functions to create their own metrics. \code{tree_metrics} will
#' dispatch the LiDAR data for each tree in the user's function. The user writes their
#' function without considering grid cells, only a cloud of points (see example).
#'
#' @param .las An object of class \code{LAS}
#' @param func the function to be applied to each tree
#' @param debug logical. If you encouter a non trivial error try \code{debug = TRUE}.
#' @return It returns a \code{data.table} containing the metrics for each tree.
#' @examples
#' LASfile <- system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, Classification = FALSE, filter = "-drop_z_below 0")
#'
#' # segment tree (refer to lastrees)
#' lastrees(las, algorithm = "li2012")
#'
#' # Max height for each tree
#' tree_metrics(las, mean(Z))
#'
#' # Define your own new metrics
#' myMetrics = function(z)
#' {
#'   metrics = list(
#'      zmean = mean(z),
#'      zmax  = max(z),
#'      npoint = length(z)
#'    )
#'
#'    return(metrics)
#' }
#'
#' metrics = tree_metrics(las, myMetrics(Intensity))
#'
#' # predefined metrics (see ?stdmetrics)
#' metrics = tree_metrics(las, .stdtreemetrics)
#' @export
tree_metrics = function(.las, func, debug = FALSE)
{
  stopifnotlas(.las)

  call = substitute(func)

  stat <- lasaggregate(.las, by = "TREE", call, NA, NA, c("tree"), FALSE, debug)

  return(stat)
}
