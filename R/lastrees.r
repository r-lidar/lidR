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
# MERCHANTABILITY or F ITNESS FOR A PARTICULAR PURPOSE.  See the
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
#' the point comes from (in a new \code{treeID} column). By default the classification is
#' done at the point cloud level and the functions retun nothing. However, with some algorithms
#' it is possible to return also a raster image of the classification and/or a
#' SpatialPolygonDataFrame. There are currently 4 algorithms implemented. See relevant sections.
#'
#' @aliases tree_segmentation
#' @param las An object of the class \code{LAS}
#' @param algorithm character. The name of an algorithm. Can be \code{"dalponte2016"},
#' \code{"watershed"},\code{"li2012"} or \code{"silva2016"} (see sections relevant to each
#' algorithm).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' about the algorithms)
#' @param extra logical. By default the functions classify the original point cloud by reference
#' and returns nothing (the original object is automatically updated in place). If
#' \code{extra = TRUE} some extra output can be returned. The type of output depends on the
#' algorithm used. It can be 1 or 2 \code{RasterLayer} or a \code{SpatialPolygonDataFrame}
#' or something else depending on intermediate objects used internally.
#' @param chm RasterLayer. Image of the canopy. You can compute it with \link{grid_canopy}
#' or \link{grid_tincanopy} or read it from external file.
#' @param treetops RasterLayer or data.frame depending either you are using a raster-based
#' or point cloud-based algorithm. You can compute it with \link{tree_detection} or read
#' it from an external file.
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the tree height multiplied by this value.
#' It should be between 0 and 1. Default 0.45
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016.  A pixel
#' is added to a region if its height is greater than the current mean height of the region
#' multiplied by this value. It should be between 0 and 1. Default 0.55
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in pixel).
#' Default 10.
#' @param max_cr_factor numeric. Maximum value of a crown diameter given as proportion of the
#' tree height. Default is 0.6 meaning 60\% of the tree height.
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012).
#' Default 1.5
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012).
#' Default 2
#' @param hmin numeric.  Minimun height of a detected tree. Default 2
#' @param R numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method. Default is 10.
#' @param tol numeric. Tolerance see ?EBImage::watershed
#' @param ext numeric. see ?EBImage::watershed
#' @return Nothing, the point cloud is updated by reference. If \code{extra = TRUE} some
#' algorithm provide extra outputs.
#'
#' @section Li 2012:
#' This method is a growthing region method working at the raw point cloud level. It is a
#' strict implementation of the Li et al. (see references) algorithm made by \code{lidR}
#' author but adding a paramter \code{hmin} to stop the segmentation for too low objects.
#' In practice this limits the over-segmentation of the method. Otherwise this algo could
#' segment a lake like a tree.
#'
#' @section Dalponte 2016:
#' This is a local maxima + growing region algorithm. It is based on the constrains proposed by
#' Dalponte and Coomes (see references). This algorithm exists in the package \code{itcSegment}.
#' This version is identical to the original but with superfluous code removed and rewritten
#' effciently. Consequently it is hundreds to millions times faster.\cr
#' Note that this algorithm strictly performs a segmentation while the original method as
#' implemented in \code{itcSegment} and described in the manuscript also performs a pre-
#' and post-process when these tasks are expected to be done by the user in separated functions.
#'
#' @section Silva 2016:
#' This is a simple but elegant method based on local maxima + voronoi tesselation described
#' in Silva et al. (2016) (see references). This algorithm is implemented in the package
#' \code{rLiDAR}. This version is \emph{not} the version from \code{rLiDAR}. This version is
#' a code written from scratch by lidR author from the original paper. Compared to the original,
#' method the algorithm works at the raw point cloud level without the need of an image of
#' the canopy. The points are classified based on the polygons contructed by the tesselation.
#' The algorithm is also considerably faster.
#'
#' @section Watershed:
#' This method is a simple \href{https://en.wikipedia.org/wiki/Watershed_(image_processing)}{watershed segmentation}
#' method. It is based on the bioconductor package \code{EBIimage}.
#' You need to install this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' # Li et al. 2012 (note: with R = 10 the result is the same
#' # but is computed 4 times slower)
#' lastrees(las, "li2012", R = 5)
#' plot(las, color = "treeID", colorPalette = col)
#'
#' chm = grid_canopy(las, res = 0.5, subcircle = 0.3)
#' chm = as.raster(chm)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' # Watershed
#' lastrees_watershed(las, chm)
#' plot(las, color = "treeID", colorPalette = pastel.colors(100))
#' plot(las, color = "treeID", colorPalette = col)
#'
#' # Dalponte 2016
#' ttops = tree_detection(chm, 5, 2)
#' lastrees_dalponte(las, chm, ttops, max_cr = 7)
#' plot(las, color = "treeID", colorPalette = col)
#'
#' @references
#' Dalponte, M. and Coomes, D. A. (2016), Tree-centric mapping of forest carbon density from
#' airborne laser scanning and hyperspectral data. Methods Ecol Evol, 7: 1236–1245. doi:10.1111/2041-210X.12575\cr\cr
#' Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A new method for segmenting individual
#' trees from the lidar point cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75-84.\cr\cr
#' Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers,
#' J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.)
#' Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573.
#' https://doi.org/10.1080/07038992.2016.1196582
#' @export
lastrees <- function(las, algorithm, ..., extra = FALSE)
{
  stopifnotlas(las)

  if (algorithm == "dalponte2016" )
    return(lastrees_dalponte(las, ...))
  else if (algorithm == "watershed")
    return(lastrees_watershed(las, ...))
  else if (algorithm == "li2012")
    return(lastrees_li(las, ...))
  else if (algorithm == "silva2016")
    return(lastrees_silva(las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

#' @export
#' @rdname lastrees
lastrees_li = function(las, dt1 = 1.5, dt2 = 2, hmin = 2, R = 10, extra = FALSE)
{
  stopifnotlas(las)

  if (dt1 <= 0) stop("dt1 should be positive",  call. = FALSE)
  if (dt1 <= 0) stop("dt1 should be positive", call. = FALSE)
  if (R <= 0)   stop("R should be positive", call. = FALSE)

  treeID   <- NULL
  progress <- LIDROPTIONS("progress")

  id = algo_li2012(las, dt1, dt2, hmin, R, progress)

  las@data[, treeID := id]

  return(invisible())
}

#' @export
#' @rdname lastrees
lastrees_watershed = function(las, chm, th_tree = 2, tol = 1, ext = 1, extra = FALSE)
{
  stopifnotlas(las)

  Canopy <- raster::as.matrix(chm)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[Canopy < th_tree] <- NA

  if (!requireNamespace("EBImage", quietly = TRUE))
    stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)

  Crowns = EBImage::watershed(Canopy, tol, ext)

  Crowns[is.na(Canopy)] <- NA
  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(chm)

  lasclassify(las, Crowns, "treeID")

  if (!extra)
    return(invisible(NULL))
  else
    return(Crowns)
}


#' @export
#' @rdname lastrees
lastrees_dalponte = function(las, chm, treetops, th_tree = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, extra = FALSE)
{
  stopifnotlas(las)

  if (!is(chm, "RasterLayer"))
    stop("chm is not a RasterLayer", call. = FALSE)

  if (!is(treetops, "RasterLayer"))
    stop("treetops is not a RasterLayer", call. = FALSE)

  if (raster::extent(chm) != raster::extent(treetops))
    stop("chm and treetops do not match together", call. = FALSE)

  if (th_seed < 0 | th_seed > 1)
    stop("th_seed should be between 0 and 1", call. = FALSE)

  if (th_cr < 0 | th_cr > 1)
    stop("th_cr should be between 0 and 1", call. = FALSE)

  Canopy <- raster::as.matrix(chm)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[is.na(Canopy)] <- -Inf

  Maxima <- raster::as.matrix(treetops)
  Maxima <- t(apply(Maxima, 2, rev))
  Maxima[is.na(Maxima)] <- 0

  Crowns = algo_dalponte(Canopy, Maxima, th_seed, th_cr, th_tree, max_cr)
  Maxima[Maxima == 0] <- NA
  Crowns[Crowns == 0] <- NA

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(chm)

  lasclassify(las, Crowns, "treeID")

  if (!extra)
    return(invisible(NULL))
  else
    return(Crowns)
}

#' @export
#' @rdname lastrees
lastrees_silva = function(las, treetops, max_cr_factor = 0.6, extra = FALSE)
{
  . <- R <- X <- Y <- Z <- NULL

  stopifnotlas(las)

  if (is(treetops, "RasterLayer"))
    stop("treetops is a RasterLayer. A data.frame is expected.", call. = FALSE)

  data.table::setDT(treetops)
  treetops[, R := Z * max_cr_factor]

  # Compute voronoi tesselation
  x = deldir::deldir(treetops$X, treetops$Y, suppressMsge = T)
  tile = deldir::tile.list(x)

  # Pre compute some sin and cos
  kcos = cos(seq(0, 2*pi, length.out = 64))
  ksin = sin(seq(0, 2*pi, length.out = 64))

  tree_polys = vector(mode = "list", length = length(tile))

  for (i in seq(along = tree_polys))
  {
    id = as.character(i)

    # Voronoi polygon coordinates
    x = tile[[i]]$x
    y = tile[[i]]$y
    x = c(x, x[1])
    y = c(y, y[1])
    xy = cbind(x,y)

    voronoi_poly = list(sp::Polygons(list(sp::Polygon(xy)), ID = id))
    voronoi_poly = sp::SpatialPolygons(voronoi_poly)

    # Disc buffer polygon coordinates
    r = treetops$R[i]
    x = r*kcos + treetops$X[i]
    y = r*ksin + treetops$Y[i]
    x = c(x, x[1])
    y = c(y, y[1])
    xy = cbind(x, y)

    disc_poly <- list(sp::Polygons(list(sp::Polygon(xy)), ID = id))
    disc_poly = sp::SpatialPolygons(disc_poly)

    # Intersection of both disc and voronoi polygon
    tree_poly = rgeos::gIntersection(voronoi_poly, disc_poly)@polygons[[1]]
    tree_poly@ID = id

    tree_polys[[i]] = tree_poly
  }

  SP   = sp::SpatialPolygons(tree_polys)
  SPDF = sp::SpatialPolygonsDataFrame(SP, data = data.frame(treeID = 1:nrow(treetops)))

  lasclassify(las, SPDF, "treeID")

  if (!extra)
    return(invisible())
  else
    return(raster::crop(SPDF, extent(las)))
}
