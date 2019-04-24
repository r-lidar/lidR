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


# ====== DALPONTE =======

#' Individual Tree Segmentation Algorithm
#'
#' This function is made to be used in \link{lastrees}. It implements an algorithm for tree
#' segmentation based on the Dalponte and Coomes (2016) algorithm (see reference).
#' This is a seeds + growing region algorithm. This algorithm exists in the package \code{itcSegment}.
#' This version has been written from the paper in C++. Consequently it is hundreds to millions times
#' faster than the original version. Note that this algorithm strictly performs a segmentation, while the
#' original method as implemented in \code{itcSegment} and described in the manuscript also performs
#' pre- and post-processing tasks. Here these tasks are expected to be done by the user in separate functions.
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. \code{lidR} is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{lastrees} to merge the result with the point
#' cloud. However the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm = raster("file/to/a/chm/")
#'  ttops = tree_detection(chm, lmf(3))
#'  crowns = dalponte2016(chm, ttops)()
#' }
#'
#' @template param-chm-lastrees
#'
#' @template param-treetops
#'
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default is 2.
#'
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the tree height multiplied by this value.
#' It should be between 0 and 1. Default is 0.45.
#'
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the current mean height of the region
#' multiplied by this value. It should be between 0 and 1. Default is 0.55.
#'
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in pixels).
#' Default is 10.
#'
#' @param ID character. If the \code{SpatialPointsDataFrame} contains an attribute with the ID for
#' each tree, the name of this attribute. This way, original IDs will be preserved. If there is no
#' such data trees will be numbered sequentially.
#'
#' @references
#' Dalponte, M. and Coomes, D. A. (2016), Tree-centric mapping of forest carbon density from
#' airborne laser scanning and hyperspectral data. Methods Ecol Evol, 7: 1236–1245. doi:10.1111/2041-210X.12575.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col <- pastel.colors(200)
#'
#' chm <- grid_canopy(las, 0.5, p2r(0.3))
#' ker <- matrix(1,3,3)
#' chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#'
#' ttops <- tree_detection(chm, lmf(4, 2))
#' las   <- lastrees(las, dalponte2016(chm, ttops))
#' plot(las, color = "treeID", colorPalette = col)
dalponte2016 = function(chm, treetops, th_tree = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, ID = "treeID")
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_all_of(treetops, "SpatialPointsDataFrame")
  assert_is_a_number(th_tree)
  assert_is_a_number(th_seed)
  assert_is_a_number(th_cr)
  assert_is_a_number(max_cr)
  assert_all_are_in_closed_range(th_seed, 0, 1)
  assert_all_are_in_closed_range(th_cr, 0, 1)

  chm      <- lazyeval::uq(chm)
  treetops <- lazyeval::uq(treetops)
  th_tree  <- lazyeval::uq(th_tree)
  th_seed  <- lazyeval::uq(th_seed)
  th_cr    <- lazyeval::uq(th_cr)
  max_cr   <- lazyeval::uq(max_cr)
  ID       <- lazyeval::uq(ID)

  f = function()
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})

    if (!is.null(context))
      stopif_wrong_context(context, c("lastrees"), "dalponte2016")

    X     <- match_chm_and_seeds(chm, treetops, ID)
    cells <- X$cells
    ids   <- X$ids

    rtreetops   <- raster::raster(chm)
    rtreetops[] <- 0L
    suppressWarnings(rtreetops[cells] <- ids)

    Canopy <- raster::as.matrix(chm)
    Canopy <- t(apply(Canopy, 2, rev))
    Canopy[is.na(Canopy)] <- -Inf

    Maxima <- raster::as.matrix(rtreetops)
    Maxima <- t(apply(Maxima, 2, rev))

    Crowns <- C_dalponte2016(Canopy, Maxima, th_seed, th_cr, th_tree, max_cr)
    Maxima[Maxima == 0L] <- NA_integer_
    Crowns[Crowns == 0L] <- NA_integer_

    Crowns <- raster::raster(apply(Crowns,1,rev))
    raster::extent(Crowns) <- raster::extent(chm)

    return(Crowns)
  }

  class(f) <- c("function", "RasterBased", "IndividualTreeSegmentation", "Algorithm", "lidR")
  return(f)
}


# ====== LI =======

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithm for tree
#' segmentation based on the Li et al. (2012) article (see reference). This method is a growing region
#' method working at the point cloud level. It is an implementation, as strict as possible, made by
#' the \code{lidR} author but with the addition of a parameter \code{hmin} to prevent over-segmentation
#' for objects that are too low.
#'
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012). Default is 1.5.
#'
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012). Default is 2.
#'
#' @param R numeric. Search radius. See page 79 in Li et al. (2012). Default is 2. If \code{R = 0}
#' all the points are automatically considered as local maxima and the search step is skipped (much
#' faster).
#'
#' @param hmin numeric. Minimum height of a detected tree. Default is 2.
#'
#' @param Zu numeric. If point elevation is greater than Zu, \code{dt2} is used, otherwise \code{dt1} is
#' used. See page 79 in Li et al. (2012). Default is 15.
#'
#' @param speed_up numeric. Maximum radius of a crown. Any value greater than a crown is
#' good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method. Default is 10.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family point-cloud based tree segmentation algorithms
#'
#' @references
#' Li, W., Guo, Q., Jakubowski, M. K., & Kelly, M. (2012). A new method for segmenting individual
#' trees from the lidar point cloud. Photogrammetric Engineering & Remote Sensing, 78(1), 75-84.
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col <- pastel.colors(200)
#'
#' las <- lastrees(las, li2012(dt1 = 1.4))
#' plot(las, color = "treeID", colorPalette = col)
li2012 = function(dt1 = 1.5, dt2 = 2, R = 2, Zu = 15, hmin = 2, speed_up = 10)
{
  assert_is_a_number(dt1)
  assert_is_a_number(dt2)
  assert_is_a_number(R)
  assert_is_a_number(Zu)
  assert_is_a_number(hmin)
  assert_is_a_number(speed_up)
  assert_all_are_positive(dt1)
  assert_all_are_positive(dt2)
  assert_all_are_non_negative(R)
  assert_all_are_positive(Zu)
  assert_all_are_positive(hmin)
  assert_all_are_positive(speed_up)

  dt1      <- lazyeval::uq(dt1)
  dt2      <- lazyeval::uq(dt2)
  R        <- lazyeval::uq(R)
  Zu       <- lazyeval::uq(Zu)
  hmin     <- lazyeval::uq(hmin)
  speed_up <- lazyeval::uq(speed_up)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lastrees"), "li2012")

    if (las@header@PHB$`Max Z` < hmin)
    {
      warning("'hmin' is higher than the highest point. No tree segmented.")
      return(rep(NA_integer_, nrow(las@data)))
    }
    else
    {
      return(C_li2012(las, dt1, dt2, Zu, R, hmin, speed_up))
    }
  }

  class(f) <- c("function", "PointCloudBased", "IndividualTreeSegmentation", "Algorithm", "lidR")

  return(f)
}

# ====== SILVA =======

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithm for tree
#' segmentation based on the Silva et al. (2016) article (see reference). This is a simple method
#' based on seed + voronoi tesselation (equivalent to nearest neighbour). This algorithm is implemented
#' in the package \code{rLiDAR}. This version is \emph{not} the version from \code{rLiDAR}. It is
#' code written from the original article by the lidR authors and is considerably (between 250
#' and 1000 times) faster.
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. \code{lidR} is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{lastrees} to merge the result into the point
#' cloud. However, the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm = raster("file/to/a/chm/")
#'  ttops = tree_detection(chm, lmf(3))
#'  crowns = silva2016(chm, ttops)()
#' }
#'
#' @template param-chm-lastrees
#'
#' @template param-treetops
#'
#' @param max_cr_factor numeric. Maximum value of a crown diameter given as a proportion of the
#' tree height. Default is 0.6, meaning 60\% of the tree height.
#'
#' @param exclusion numeric. For each tree, pixels with an elevation lower than \code{exclusion}
#' multiplied by the tree height will be removed. Thus, this number belongs between 0 and 1.
#'
#' @param ID character. If the \code{SpatialPointsDataFrame} contains an attribute with the ID for
#' each tree, the name of this column. This way, original IDs will be preserved. If there is no such
#' data trees will be numbered sequentially.
#'
#' @references
#' Silva, C. A., Hudak, A. T., Vierling, L. A., Loudermilk, E. L., O’Brien, J. J., Hiers,
#' J. K., Khosravipour, A. (2016). Imputation of Individual Longleaf Pine (Pinus palustris Mill.)
#' Tree Attributes from Field and LiDAR Data. Canadian Journal of Remote Sensing, 42(5), 554–573.
#' https://doi.org/10.1080/07038992.2016.1196582.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col <- pastel.colors(200)
#'
#' chm <- grid_canopy(las, res = 0.5, p2r(0.3))
#' ker <- matrix(1,3,3)
#' chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#'
#' ttops <- tree_detection(chm, lmf(4, 2))
#' las   <- lastrees(las, silva2016(chm, ttops))
#' plot(las, color = "treeID", colorPalette = col)
silva2016 = function(chm, treetops, max_cr_factor = 0.6, exclusion = 0.3, ID = "treeID")
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_all_of(treetops, "SpatialPointsDataFrame")
  assert_is_a_number(max_cr_factor)
  assert_is_a_number(exclusion)
  assert_all_are_in_open_range(max_cr_factor, 0, 1)
  assert_all_are_in_open_range(exclusion, 0, 1)

  chm            <- lazyeval::uq(chm)
  treetops       <- lazyeval::uq(treetops)
  max_cr_factor  <- lazyeval::uq(max_cr_factor)
  exclusion      <- lazyeval::uq(exclusion)
  ID             <- lazyeval::uq(ID)

  f = function()
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})

    if (!is.null(context))
      stopif_wrong_context(context, c("lastrees"), "silva2016")

    . <- R <- X <- Y <- Z <- id <- d <- hmax <- NULL

    X     <- match_chm_and_seeds(chm, treetops, ID)
    cells <- X$cells
    ids   <- X$ids

    chmdt <- data.table::setDT(raster::as.data.frame(chm, xy = TRUE, na.rm = T))
    data.table::setnames(chmdt, names(chmdt), c("X", "Y", "Z"))

    # Voronoi tesselation is nothing else but the nearest neigbour
    u <- C_knn(treetops@coords[,1], treetops@coords[,2], chmdt$X, chmdt$Y, 1L, getThread())

    chmdt[, id := u$nn.idx[,1]]
    chmdt[, id := ids[id]]
    chmdt[, d := u$nn.dist[,1]]
    chmdt[, hmax := max(Z), by = id]

    chmdt   <- chmdt[Z >= exclusion*hmax & d <= max_cr_factor*hmax, .(X,Y, id)]
    crown   <- chm
    suppressWarnings(crown[] <- NA_integer_)
    cells   <- raster::cellFromXY(crown, chmdt[, .(X,Y)])
    suppressWarnings(crown[cells] <- chmdt[["id"]])

    return(crown)
  }

  class(f) <- c("function", "RasterBased", "IndividualTreeSegmentation", "OpenMP", "Algorithm", "lidR")
  return(f)
}

# ====== WATERSHEDS =======

#' Individual Tree Segmentation Algorithm
#'
#' This function is made to be used in \link{lastrees}. It implements an algorithm for tree
#' segmentation based on a watershed or a marker-controlled watershed.
#' \itemize{
#' \item \strong{Simple watershed} is based on the bioconductor package \code{EBIimage}. You need to install
#' this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#' Internally, the function EBImage::watershed is called.
#' \item \strong{Marker-controlled watershed} is based on the \code{imager} package. Internally, the
#' function \link[imager:watershed]{imager::watershed} is called using the tree tops as a priority map.
#' }
#'
#' Because this algorithm works on a CHM only there is no actual need for a point cloud. Sometimes the
#' user does not even have the point cloud that generated the CHM. \code{lidR} is a point cloud-oriented
#' library, which is why this algorithm must be used in \link{lastrees} to merge the result into the point
#' cloud. However, the user can use this as a stand-alone function like this:
#' \preformatted{
#'  chm = raster("file/to/a/chm/")
#'  ttops = tree_detection(chm, lmf(3))
#'  crowns = watershed(chm)()
#' }
#'
#' @template param-chm-lastrees
#'
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default is 2.
#'
#' @param tol numeric. Tolerance see ?EBImage::watershed.
#'
#' @param ext numeric. see ?EBImage::watershed.
#'
#' @template param-treetops
#'
#' @param ID character. If the \code{SpatialPointsDataFrame} contains an attribute with the ID for
#' each tree, the name of this column. This way, original IDs will be preserved. If there is no such
#' data trees will be numbered sequentially.
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family raster based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col <- pastel.colors(250)
#'
#' chm <- grid_canopy(las, res = 0.5, p2r(0.3))
#' ker <- matrix(1,3,3)
#' chm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
#'
#' ttops <- tree_detection(chm, lmf(4, 2))
#' las   <- lastrees(las, mcwatershed(chm, ttops))
#'
#' x = plot(las, color = "treeID", colorPalette = col)
#' add_treetops3d(x, ttops)
watershed = function(chm, th_tree = 2, tol = 1, ext = 1)
{
  chm     <- lazyeval::uq(chm)
  th_tree <- lazyeval::uq(th_tree)
  tol     <- lazyeval::uq(tol)
  ext     <- lazyeval::uq(ext)

  ws_generic(chm, th_tree = th_tree, tol = tol, ext = ext)
}

#' @rdname watershed
#' @export
mcwatershed = function(chm, treetops, th_tree = 2, ID = "treeID")
{
  chm      <- lazyeval::uq(chm)
  treetops <- lazyeval::uq(treetops)
  th_tree  <- lazyeval::uq(th_tree)
  ID       <- lazyeval::uq(ID)

  ws_generic(chm, th_tree = th_tree, treetops = treetops, ID = ID)
}

ws_generic = function(chm, th_tree = 2, tol = 1, ext = 1, treetops = NULL, ID = "treeID")
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_a_number(th_tree)
  assert_is_a_number(tol)
  assert_is_a_number(ext)

  f = function()
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})

    if (!is.null(context))
      stopif_wrong_context(context, c("lastrees"), "watershed")

    # Test if requiered packages are installed
    if (is.null(treetops))
    {
      if (!requireNamespace("EBImage", quietly = TRUE))
        stop("'EBImage' package is needed for this function to work. Please read documentation.")
    }
    else
    {
      if (!requireNamespace("imager", quietly = TRUE))
        stop("'imager' package is needed for this function to work. Please read documentation.")
    }

    # Convert the CHM to a matrix
    Canopy <- raster::as.matrix(chm)
    mask   <- Canopy < th_tree | is.na(Canopy)
    Canopy[mask] <- 0

    # Watershed
    if (is.null(treetops))
    {
      Crowns = EBImage::watershed(Canopy, tol, ext)
    }
    # Marker-controlled watershed
    else
    {
      X = match_chm_and_seeds(chm, treetops, ID)
      cells = X$cells
      ids = X$ids

      seeds = chm
      seeds[] = 0L
      seeds[cells] = ids
      treetops = raster::as.matrix(seeds)

      Canopy <- imager::as.cimg(Canopy)
      treetops  <- imager::as.cimg(treetops)
      Crowns <- imager::watershed(treetops, Canopy)
      Crowns <- Crowns[,,1,1]
      storage.mode(Crowns) = "integer"
    }

    Crowns[mask] <- NA_integer_
    Crowns = raster::raster(Crowns)
    raster::extent(Crowns) = raster::extent(chm)

    return(Crowns)
  }

  class(f) <- c("function", "RasterBased", "IndividualTreeSegmentation", "Algorithm", "lidR")
  return(f)
}
