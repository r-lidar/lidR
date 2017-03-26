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
#' Individual tree segmentation with several possible algorithms (see details). The function
#' attributes to each point of the point cloud a number identifying the detected tree
#' the point comes from (\code{treeID} column). By default the classification is done at the
#' point cloud level. However, with some algorithms it is possible to return a raster image of the
#' classification. There are currently 3 algorithms implemented. See relevant sections.
#'
#' @param .las An object of the class \code{LAS}
#' @param algorithm character. The name of an algorithm. Can be \code{"dalponte2016"},
#' \code{"watershed"} or \code{"li2012"} (see sections relevant to each algorithm).
#' @param image RasterLayer. Image of the canopy if the algorithm works on a canopy surface model.
#' But some algorithms work on the raw point cloud (see relevant sections). You can compute
#' it with \link{grid_canopy} or read it from external file.
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details about the algorithms)
#' @param extra logical. By default the function works at the point cloud level and returns nothing.
#' If \code{extra = TRUE} the function can return a \link[raster:raster]{RasterLayer} or a list of 2 RasterLayers
#' with the positions of the local maxima and a map of the crowns, depending on the algorithm used.
#'
#' @return Nothing, the point cloud is updated by reference. If \code{extra = TRUE},
#' \code{"dalponte2012"} returns two RasterLayers, \code{"watershed"} returns one RasterLayer
#' and \code{"Li2012"} does not support the \code{extra} parameter.
#'
#' @section Dalponte 2016:
#'
#' This is the algorithm developed by Dalponte and Coomes (see references). This algorithm exists
#' in the package \pkg{itcSegment}. This version is identical to the original but with
#' superfluous code removed and rewritten in C++. Consequently it is 6 times faster.
#' Note that this algorithm strictly performs a segmentation while the original method as implemented
#' in \code{itcSegment} and described in the manuscript also performs a pre- and post-process when
#' these tasks are expected to be done by the user.
#' The names of the parameters are the same as those in Dalponte's \code{itcSegment} package.
#' Dalponte's algorithm is a canopy surface model-based method. An image of the canopy is
#' expected.
#' \describe{
#' \item{\code{searchWinSize}}{Size (in pixels) of the moving window used to the detect the
#' local maxima. It should be an odd number larger than 3. Default 3}
#' \item{\code{TRESHSeed}}{Growing threshold 1. It should be between 0 and 1. Default 0.45}
#' \item{\code{TRESHCrown}}{Growing threshold 2. It should be between 0 and 1. Default 0.55}
#' \item{\code{DIST}}{Maximum value of the crown diameter of a detected tree (in meters). Default 10}
#' \item{\code{th}}{Digital number value below which a pixel cannot be a local maxima. Default 2}
#' }
#'
#' @section Watershed:
#'
#' This method relies on the \href{https://en.wikipedia.org/wiki/Watershed_(image_processing)}{watershed segmentation}
#' method. It is based on the bioconductor package \pkg{EBIimage}. You need to install
#' this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#' The Watershed algorithm is a canopy surface model-based method. An image of the canopy is
#' expected.
#' \describe{
#' \item{\code{th}}{Numeric. Number value below which a pixel cannot be a crown. Default 2}
#' \item{\code{tolerance}}{Numeric. see ?EBImage::watershed}
#' \item{\code{ext}}{Numeric. see ?EBImage::watershed}
#' }
#'
#' @section Li 2012:
#'
#' This method is an implementation of the Li et al. (see references) algorithm made by \pkg{lidR}
#' author. It may have some differences compared with the original method due to potential  mis-interpretation
#' of the Li et al. manuscript. This method works at the point cloud level. An
#' image of the canopy is \emph{not} expected.
#' \describe{
#' \item{\code{dt1}}{Numeric. Threshold number 1. See reference page 79. Default is 1.5}
#' \item{\code{dt2}}{Numeric. Threshold number 2. See reference page 79. Default is 2}
#' \item{\code{R}}{Numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method. Default is 10.}
#' }
#' The current implementation is known to be slow. Improvements are possible in future
#' package versions.
#'
#' @examples
#' LASfile <- system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, XYZonly = TRUE, filter = "-drop_z_below 0")
#'
#' # compute a canopy image
#' chm = grid_canopy(las, res = 0.5, subcircle = 0.1, na.fill = "knnidw", k = 4)
#' chm = as.raster(chm)
#'
#' # smoothing post-process (e.g. 2x mean)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean)
#' chm = raster::focal(chm, w = kernel, fun = mean)
#' raster::plot(chm, col = height.colors(50)) # check the image
#'
#' # segmentation
#' lastrees(las, "dalponte2016", chm, th = 5)
#'
#' # plot points that actually are trees
#' trees = lasfilter(las, !is.na(treeID))
#' plot(trees, color = "treeID", colorPalette = random.colors(100))
#'
#' @references
#' Dalponte, M. and Coomes, D. A. (2016), Tree-centric mapping of forest carbon density from
#' airborne laser scanning and hyperspectral data. Methods Ecol Evol, 7: 1236–1245. doi:10.1111/2041-210X.12575\cr\cr
#' Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A new method for segmenting individual
#' trees from the lidar point cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75-84.
#' @export
lastrees <-function(.las, algorithm, image = NULL, ..., extra = FALSE)
{
  if(algorithm == "dalponte2016" )
    return(dalponte2012(.las, image, extra, ...))
  else if(algorithm == "watershed")
    return(watershed(.las, image, extra, ...))
  else if (algorithm == "li2012")
    return(li2012(.las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

dalponte2012 = function(.las, image, extra, searchWinSize = 3, TRESHSeed = 0.45, TRESHCrown = 0.55, DIST = 10, th = 2)
{
  if (searchWinSize < 3 | searchWinSize %% 2 == 0)
    stop("searchWinSize not correct", call. = FALSE)

  l = dim(image)[1]
  w = dim(image)[2]

  Canopy <- raster::as.matrix(image)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[is.na(Canopy) | Canopy < th] <- 0

  Maxima = itc_treetops(Canopy, searchWinSize)
  Crowns = itc_expandcrowns(Canopy, Maxima, TRESHSeed, TRESHCrown, DIST)
  Maxima[Maxima == 0] <- NA
  Crowns[Crowns == 0] <- NA

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(image)

  lasclassify(.las, Crowns, "treeID")

  if(extra == FALSE)
    return(invisible(NULL))
  else
  {
    Maxima = raster::raster(apply(Maxima,1,rev))
    raster::extent(Maxima) = raster::extent(image)

    return(list(Crown = Crowns, Maxima = Maxima))
  }
}

li2012 = function(.las, dt1 = 1.5, dt2 = 2, R = 10)
{
  treeID <- NULL

  data.table::setorderv(.las@data, "Z", order=-1L)

  id = algo_li2012(.las@data$X, .las@data$Y, .las@data$Z, c(dt1, dt2), R = R)

  .las@data[, treeID := id][]

  return(invisible())
}

watershed = function(.las, image, extra, th = 2, tolerance = 1, ext = 1)
{
  l = dim(image)[1]
  w = dim(image)[2]

  Canopy <- raster::as.matrix(image)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[Canopy < th] <- NA

  if (!requireNamespace("EBImage", quietly = TRUE))
    stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)

  Crowns = EBImage::watershed(Canopy, tolerance, ext)

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(image)

  lasclassify(.las, Crowns, "treeID")

  if(extra == FALSE)
    return(invisible(NULL))
  else
  {
    return(Crowns)
  }
}