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

#' Individual tree segmentation
#'
#' Individual tree segmentation with several possible algorithm (see details). The function
#' attribute to each point of the point cloud an number to identify from with detected tree
#' the point comes from. By defaut the classification is done at the point level. However it is possible
#' to return a raster image of the classification with some algoithms. There are currently 1 algorithm
#' implemented. See relative sections
#'
#' @section Dalponte 2012:
#'
#' \code{algorithm = "dalponte2012"}
#'
#' This is the algorithm developped by M. Dalponte (see references). This algorithm exists
#' in the package \code{itcSegment}. This version is stricly identical to the original one but had entierly been
#' cleaned of useless code and rewritten in pure C++. It is 6 times faster. The name of the paramters are those
#' you can find in orginal Dalponte's code in \code{itcSegment} package. Dalponte's algorithm is a canopy
#' surface model based method. An image of the canopy is expected.
#' \describe{
#' \item{\code{searchWinSize}}{Size (in pixels) of the moving window used to the detect the local maxima. It should be an odd number larger than 3. Default 3}
#' \item{\code{TRESHSeed}}{Growing threshold 1. It should be between 0 and 1. Default 0.45}
#' \item{\code{TRESHCrown}}{Growing threshold 2. It should be between 0 and 1. Default 0.55}
#' \item{\code{DIST}}{Maximum value of the crown diameter of a detected tree (in meters). Default 10}
#' \item{\code{th}}{Digital number value below which a pixel cannot be a local maxima. Default 2}
#' }
#'
#' @section Li 2012:
#'
#' Not yet implemented
#'
#' @param .las An object of the class \code{LAS}
#' @param algorithm string. The name of an algorithm. Can be \code{"dalponte2012"}, \code{"li2012"}.
#' (see sections relative to each algorithm)
#' @param image an image of the canopy if the algorithm works on canopy surface model.
#' but some algorithms work on the raw point cloud. You can compute it with grid_canopy or read it from external file.
#' @param ... parameter for the algorithms. Depends on the algorithm used (see details about the algoritms)
#' @param extra logical. By defaut the function works at the point cloud level and return nothing.
#' If \code{extra = TRUE} the function return a list of 2 \link[raster:raster]{RasterLayer} with the position
#' of the local maxima and the map of the crowns.
#'
#' @references
#' M. Dalponte, F. Reyes, K. Kandare, and D. Gianelle, "Delineation of Individual Tree Crowns from ALS and Hyperspectral data: a comparison among four methods," European Journal of Remote Sensing, Vol. 48, pp. 365-382, 2015.
#' @export
lastrees <-function(.las, algorithm, image = NULL, extra = FALSE, ...)
{
  if(algorithm == "dalponte2012" )
    return(dalponte2012(.las, image, extra, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

dalponte2012 = function(.las, image, extra, searchWinSize = 3, TRESHSeed = 0.45, TRESHCrown = 0.55, DIST = 10, th = 2)
{
  if (searchWinSize < 3 | searchWinSize %% 2 == 0)
    stop("searchWinSize not correct", call. = FALSE)

  l = dim(image)[1]
  w = dim(image)[2]

  Canopy <- matrix(w, l, data = image, byrow = FALSE)
  Canopy <- Canopy[1:w, l:1]
  Canopy[is.na(Canopy) | Canopy < th] <- 0

  Maxima = itc_treetops(Canopy, searchWinSize)
  Crowns = itc_expandcrowns(Canopy, Maxima, TRESHSeed, TRESHCrown, DIST)

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(image)

  lasclassify(.las, Crowns, "treeID")

  if(extra == FALSE)
    return(invisible(NULL))
  else
  {
    Maxima = raster::raster(apply(Maxima,1,rev))
    raster::extent(Maxima) = raster::extent(image)

    return(list(Crowns, Maxima))
  }
}