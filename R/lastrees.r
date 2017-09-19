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
#' @param las An object of the class \code{LAS}
#' @param algorithm character. The name of an algorithm. Can be \code{"dalponte2016"},
#' \code{"watershed"},\code{"li2012"} or \code{"silva2016"} (see sections relevant to each
#' algorithm).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' about the algorithms)
#' @param img RasterLayer. Image of the canopy if the algorithm works on a canopy surface
#' model. But some algorithms work on the raw point cloud (see relevant sections). You can
#' compute it with \link{grid_canopy} or \link{grid_tincanopy} or read it from external file.
#' @param lm_ws numeric. Size of the moving window used to the detect the local maxima. On a
#' raster this size is in pixel and  should be an odd number larger than 3. On a raw point cloud
#' this size is in the point cloud units (usually meters but sometime feets).
#' @param th_lm numeric. Threshold below which a pixel or a point cannot be a local maxima. Default 2.
#' @param th_tree numeric. Threshold below which a point cannot be classified a tree. Default is 2.
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. It
#' should be between 0 and 1. Default 0.45
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016. It
#' should be between 0 and 1. Default 0.55
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in meters).
#' Default 10.
#' @param cr_factor numeric. Maximum value of a crown diameter given as proportion of the
#' tree height. Default is 0.6 meaning 60\% of the tree height.
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012). Default 1.5
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012). Default 2
#' @param seep_up numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method. Default is 10.
#' @param tol numeric. Tolerance see ?EBImage::watershed
#' @param ext numeric. see ?EBImage::watershed
#' @param extra logical. By default the function classify the orinal point cloud by reference and returns
#' nothing. If \code{extra = TRUE} the function can return extra gift depending on the algorithm used.
#' @return Nothing, the point cloud is updated by reference. If \code{extra = TRUE} some algorithm provide
#' extra outputs.
#'
#' @section Dalponte 2016:
#' This is a local maxima + growing region algorithm. It is based on the algorithm developed by
#' Dalponte and Coomes (see references). This algorithm exists in the package \code{itcSegment}.
#' This version is identical to the original but with superfluous code removed and rewritten
#' in C++. Consequently it is 6 times faster.\cr
#' Note that this algorithm strictly performs a segmentation while the original method as implemented
#' in \code{itcSegment} and described in the manuscript also performs a pre- and post-process when
#' these tasks are expected to be done by the user.\cr
#' Dalponte's algorithm is a canopy surface model-based method. An image of the canopy is
#' expected.
#'
#' @section Silva 2016:
#' This is an simple but elegant method based on local maxima + voronoi tesselation discribed
#' in Silva et al. (2016) (see references). This algorithm is implemented in the package
#' \code{rLiDAR}. This version is \emph{not} the version from \code{rLiDAR}. This version is
#' a version written in C++ and made by lidR developper. Moreover the method works at the
#' raw point cloud level without need of an image of the canopy compared to the original
#' algorithm. The local maxima is computed at the raw point cloud level without raster as
#' well as the crown segmentation in the voroinoi tesselation.
#'
#' @section Watershed:
#' This method is a simple (\href{https://en.wikipedia.org/wiki/Watershed_(image_processing)}{watershed segmentation})
#' method. It is based on the bioconductor package \code{EBIimage}. You need to install
#' this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#' The Watershed algorithm is a canopy surface model-based method. An image of the canopy is
#' expected.
#'
#' @section Li 2012:
#' This method is a growthing region method working at the raw point cloud level. It is an
#' implementation of the Li et al. (see references) algorithm made by \code{lidR}
#' author. This method works at the point cloud level. An image of the canopy is \emph{not}
#' expected.
#' @examples
#' LASfile <- system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # segmentation
#' lastrees(las, "li2012")
#'
#' # plot points that actually are trees
#' trees = lasfilter(las, !is.na(treeID))
#' plot(trees, color = "treeID", colorPalette = random.colors(100))
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
lastrees <- function(las, algorithm, ...)
{
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
lastrees_watershed = function(las, img, th_cr = 2, tol = 1, ext = 1)
{
  l = dim(image)[1]
  w = dim(image)[2]

  Canopy <- raster::as.matrix(image)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[Canopy < th_cr] <- NA

  if (!requireNamespace("EBImage", quietly = TRUE))
    stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)

  Crowns = EBImage::watershed(Canopy, tol, ext)

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(image)

  lasclassify(.las, Crowns, "treeID")

  if (extra == FALSE)
    return(invisible(NULL))
  else
    return(Crowns)
}


#' @export
#' @rdname lastrees
lastrees_dalponte = function(las, img, lm_ws = 3, th_lm = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, th_tree = 2)
{
  if (searchWinSize < 3 | searchWinSize %% 2 == 0)
    stop("searchWinSize not correct", call. = FALSE)

  l = dim(image)[1]
  w = dim(image)[2]

  Canopy <- raster::as.matrix(image)
  Canopy <- t(apply(Canopy, 2, rev))
  Canopy[is.na(Canopy) | Canopy < th_lm] <- 0

  Maxima = LocalMaximaMatrix(Canopy, searchWinSize)
  Crowns = itc_expandcrowns(Canopy, Maxima, th_seed, th_cr, max_cr)
  Maxima[Maxima == 0] <- NA
  Crowns[Crowns == 0] <- NA

  Crowns = raster::raster(apply(Crowns,1,rev))
  raster::extent(Crowns) = raster::extent(image)

  lasclassify(las, Crowns, "treeID")
  las@data[Z < th_tree, treeID := NA][]

  if (extra == FALSE)
    return(invisible(NULL))
  else
  {
    Maxima = raster::raster(apply(Maxima,1,rev))
    raster::extent(Maxima) = raster::extent(image)

    return(list(Crown = Crowns, Maxima = Maxima))
  }
}

#' @export
#' @rdname lastrees
lastrees_li = function(las, dt1 = 1.5, dt2 = 2, th_tree = 2, seep_up = 10)
{
  treeID <- NULL

  if (dt1 > dt2)     stop("dt1 greater than dt2", call. = FALSE)
  if (seep_up <= 0)  stop("seep_up <= 0", call. = FALSE)

  data.table::setorderv(las@data, "Z", order = -1L)

  id = algo_li2012(las@data$X, las@data$Y, las@data$Z, dt1, dt2, R = seep_up, LIDROPTIONS("progress"))

  las@data[, treeID := id]
  las@data[Z < th_tree, treeID := NA][]

  return(invisible())
}

#' @export
#' @rdname lastrees
lastrees_silva = function(las, lm_ws = 5, cr_factor = 0.6, th_lm = 2, th_tree = 2)
{
  # search local maxima
  maxima  = las@data %$% LocalMaximaPoints(X, Y, Z, lm_ws/2, LIDROPTIONS("progress"))
  filter  = las@data$Z > th_lm
  maxima  = maxima & filter

  locmax  = las@data[maxima == TRUE, .(X, Y, Z)]
  locmax[, R := Z * cr_factor]

  # Compute voronoi tesselation
  x = deldir::deldir(locmax$X, locmax$Y, suppressMsge = T)
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
    r = locmax$R[i]
    x = r*kcos + locmax$X[i]
    y = r*ksin + locmax$Y[i]
    x = c(x, x[1])
    y = c(y, y[1])
    xy = cbind(x, y)

    disc_poly <- list(sp::Polygons(list(sp::Polygon(xy)), ID = id))
    disc_poly = sp::SpatialPolygons(disc_poly)

    # Intersection of both disc and voronoi polygon
    tree_poly = rgeos::gIntersection(voronoi_poly, disc_poly)@polygons[[1]]
    tree_poly@ID = id

    tree_polys[[i]] = poly
  }

  SP   = sp::SpatialPolygons(polys)
  SPDF = sp::SpatialPolygonsDataFrame(SP, data = data.frame(treeID = 1:nrow(locmax)))

  lasclassify(las, SPDF, "treeID")
  las@data[!filter, treeID := NA][]
  return(invisible())
}
