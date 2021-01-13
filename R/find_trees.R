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
#' @param las An object of class `LAS` or `LAScatalog`. Can also be a `RasterLayer`
#' representing a canopy height model, in which case it is processed like a regularly-spaced point cloud.
#' @param algorithm An algorithm for individual tree detection. lidR has: \link{lmf} and \link{manual}.
#' More experimental algorithms may be found in the package \href{https://github.com/Jean-Romain/lidRplugins}{lidRplugins}.
#' @param uniqueness character. A method to compute a unique ID. Can be 'incremental', 'gpstime' or
#' 'bitmerge'. See section 'Uniqueness'. This feature must be considered as 'experimental'.
#'
#' @template section-uniqueness
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
#' las <- readLAS(LASfile, select = "xyz", filter = "-inside 481250 3812980 481300 3813030")
#'
#' ttops <- find_trees(las, lmf(ws = 5))
#'
#' #x = plot(las)
#' #add_treetops3d(x, ttops)
#' @md
find_trees = function(las, algorithm, uniqueness = 'incremental')
{
  UseMethod("find_trees", las)
}

#' @export
find_trees.LAS = function(las, algorithm, uniqueness = 'incremental')
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_itd(algorithm)
  match.arg(uniqueness, c('incremental', 'gpstime', 'bitmerge'))

  if (uniqueness == 'gpstime' && !"gpstime" %in% names(las@data))
    stop("Impossible to compute unique IDs using gpstime: no gpstime found.", call. = FALSE)

  if (uniqueness == 'gpstime' &&  fast_countequal(las@data[["gpstime"]], 0L) == npoints(las))
    stop("Impossible to compute unique IDs using gpstime: gpstime is not populated.", call. = FALSE)

  lidR.context <- "find_trees"
  res <- algorithm(las)

  if (is(res, "SpatialPointsDataFrame"))
    return(res)

  if (is.logical(res) || is.integer(res))
  {
    maxima <- las@data[res, c("X", "Y", "Z")]

    if (nrow(maxima) == 0)
    {
      coords <- matrix(0, ncol = 2)
      data   <- data.frame(treeID = integer(1), Z = numeric(1))
      output <- sp::SpatialPointsDataFrame(coords, data, proj4string = las@proj4string)
      output <- output[0,]
    }
    else
    {
      coords <- cbind(maxima[["X"]], maxima[["Y"]])

      if (uniqueness == "incremental")
      {
        ids <- 1:nrow(maxima)
      }
      else if (uniqueness == "gpstime")
      {
        ids <- las@data[["gpstime"]][res]
      }
      else
      {
        xoffset <- las@header@PHB[["X offset"]]
        yoffset <- las@header@PHB[["Y offset"]]
        zoffset <- las@header@PHB[["Z offset"]]

        xscale  <- las@header@PHB[["X scale factor"]]
        yscale  <- las@header@PHB[["Y scale factor"]]
        zscale  <- las@header@PHB[["Z scale factor"]]

        xscaled <- as.integer((maxima[["X"]] - xoffset)/xscale)
        yscaled <- as.integer((maxima[["Y"]] - yoffset)/yscale)

        ids <- xscaled * 2^32 + yscaled
      }

      data   <- data.frame(treeID = ids, Z = maxima[["Z"]])
      output <- sp::SpatialPointsDataFrame(coords, data, proj4string = las@proj4string)
    }

    output@bbox <- sp::bbox(las)
    return(output)
  }

  stop("The output of the algorithm is incorrect")
}

#' @export
find_trees.RasterLayer = function(las, algorithm, uniqueness = 'incremental')
{
  data <- raster::as.data.frame(las, xy = TRUE, na.rm = TRUE)
  names(data) <- c("X", "Y", "Z")
  header <- rlas::header_create(data)
  las <- LAS(data, header, proj4string = las@crs, check = FALSE)
  return(find_trees(las, algorithm))
}

#' @export
find_trees.LAScluster = function(las, algorithm, uniqueness = 'incremental')
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  ttops <- find_trees(x, algorithm, uniqueness)
  bbox  <- raster::extent(las)
  ttops <- raster::crop(ttops, bbox)
  return(ttops)
}

#' @export
find_trees.LAScatalog = function(las, algorithm, uniqueness = 'incremental')
{
  if (uniqueness == "gpstime")
    opt_select(las) <- "xyzt"
  else
    opt_select(las) <- "xyz"

  options <- list(need_buffer = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, find_trees, algorithm = algorithm, uniqueness = uniqueness, .options = options)
  return(output)
}


