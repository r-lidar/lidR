# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017-2018 Jean-Romain Roussel
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

#' Individual tree detection
#'
#' Individual tree detection function that find the position of the trees using several possible
#' algorithms.
#'
#' @param las An object of class \code{LAS} or \code{LAScatalog}. Can also be a \code{RasterLayer}
#' representing a canopy height model, in which case it is processed like a regularly-spaced point cloud.
#'
#' @param algorithm An algorithm for individual tree detection. lidR has: \link{lmf} and \link{manual}.
#' More experimental algorithms may be found in the package \href{https://github.com/Jean-Romain/lidRplugins}{lidRplugins}.
#'
#' @template section-supported-option-tree_detection
#'
#' @return A SpatialPointsDataFrame with an attribute Z for the tree tops and treeID with an individual
#' ID for each tree.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' ttops <- tree_detection(las, lmf(ws = 5))
#'
#' x = plot(las)
#' add_treetops3d(x, ttops)
tree_detection = function(las, algorithm)
{
  UseMethod("tree_detection", las)
}

#' @export
tree_detection.LAS = function(las, algorithm)
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.")

  if (!is(algorithm, "IndividualTreeDetection"))
    stop("The algorithm used is not an algorithm for individual tree detection.")

  lidR.context = "tree_detection"
  return(algorithm(las))
}

#' @export
tree_detection.RasterLayer = function(las, algorithm)
{
  lidR.context <- "tree_detection"
  y = raster::as.data.frame(las, xy = TRUE, na.rm = TRUE)
  data.table::setDT(y)
  data.table::setnames(y, names(y), c("X", "Y", "Z"))
  las = LAS(y, proj4string = las@crs)
  return(algorithm(las))
}

#' @export
tree_detection.LAScluster = function(las, algorithm)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  ttops <- tree_detection(x, algorithm)
  bbox  <- raster::extent(las)
  ttops <- raster::crop(ttops, bbox)
  return(ttops)
}

#' @export
tree_detection.LAScatalog = function(las, algorithm)
{
  opt_select(las) <- "xyz"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = FALSE)
  output  <- catalog_apply(las, tree_detection, algorithm = algorithm, .options = options)

  if (opt_output_files(las) == "")
  {
    output  <- do.call(rbind, output)
    output@proj4string <- las@proj4string
    output@data$treeID <- 1:length(output@data$treeID)
  }
  else
  {
    output <- unlist(output)
  }

  return(output)
}

