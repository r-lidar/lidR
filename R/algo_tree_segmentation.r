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

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithms for tree
#' segmentation based a paper published by Dalponte and Coomes (2016) algorithm (see reference).
#' This is a seeds + growing region algorithm. This algorithm exists in the package \code{itcSegment}.
#' This version has been written from the paper in C++. Consequently it is hundreds to millions times
#' faster to the original one. Notethat this algorithm strictly performs a segmentation, while the
#' original method as implemented in \code{itcSegment} and described in the manuscript also performs
#' a pre- and post-process when these tasks are expected to be done by the user in separate functions.
#'
#' Because this algorithm works on a CHM only there is no actual need of a point cloud. Sometime the
#' user do not even have the point cloud that generated the CHM. \code{lidR} is a point cloud oriented
#' library this is why this algorithm must be used in \link{lastrees} to reafect the result into the point
#' cloud. However the user can use this function alone like that:
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
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
#'
#' @param th_seed numeric. Growing threshold 1. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the tree height multiplied by this value.
#' It should be between 0 and 1. Default 0.45.
#'
#' @param th_cr numeric. Growing threshold 2. See reference in Dalponte et al. 2016. A pixel
#' is added to a region if its height is greater than the current mean height of the region
#' multiplied by this value. It should be between 0 and 1. Default 0.55.
#'
#' @param max_cr numeric. Maximum value of the crown diameter of a detected tree (in pixels).
#' Default 10.
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
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' chm = grid_canopy(las, 0.5, p2r(0.3))
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, lmf(4, 2))
#' lastrees(las, dalponte2016(chm, ttops))
#' plot(las, color = "treeID", colorPalette = col)
dalponte2016 = function(chm, treetops, th_tree = 2, th_seed = 0.45, th_cr = 0.55, max_cr = 10, ID = "treeID")
{
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_all_of(treetops, "SpatialPointsDataFrame")
  assertive::assert_is_a_number(th_tree)
  assertive::assert_is_a_number(th_seed)
  assertive::assert_is_a_number(th_cr)
  assertive::assert_is_a_number(max_cr)
  assertive::assert_all_are_in_closed_range(th_seed, 0, 1)
  assertive::assert_all_are_in_closed_range(th_cr, 0, 1)

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

    Crowns <- C_lastrees_dalponte(Canopy, Maxima, th_seed, th_cr, th_tree, max_cr)
    Maxima[Maxima == 0L] <- NA_integer_
    Crowns[Crowns == 0L] <- NA_integer_

    Crowns <- raster::raster(apply(Crowns,1,rev))
    raster::extent(Crowns) <- raster::extent(chm)

    return(Crowns)
  }

  class(f) <- c("function", "RasterBased", "IndividualTreeSegmentation", "Algorithm", "lidR")
  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithms for tree
#' segmentation based on paper written by Hamraz et al. (2012). See references and details.
#'
#' This function has been written by the \code{lidR} authors from the original article. We made our
#' best to implement as far as possible exactly what is written in the original paper but we cannot
#' affirm that it is this exact original algorithm.\cr\cr
#' Also it is important to notice that we have never been able to segment tree properly with this
#' method. The important sensitivity to minor deviation as well as the great number of difficultly
#' parametrizable imputs lead us to a method that we are not able to use ourselves.\cr\cr
#' Also, minor variations were introduced to fix some issues that were not adressed in the original paper.
#' Because the methods described in section 2.2 of the original article appear extremely sensitive to
#' many minor deviations, we introduced an optionnal additionnal step to clean the profiles used to build
#' the convex hull. When \code{filter_profiles = TRUE}, profiles that discribe a crown radius smaller than
#' \code{2*nps} (basically radius that are close to 0) are removed and the convex hull is build considering
#' the profiles that describe a crown radius comprise between the 10th and the 90th of the radiuses found.
#' This enable to remove outliers and reduce dummy segmentation but anyway we were not able to segment
#' the tree properly with this method.\cr\cr
#' As a conclusion this algorithm might be considered as a free and open source code provided to be
#' improved by the community. One can check and study the sources to ensure that it correspond to the
#' original paper and find potential improvement.\cr\cr
#' Also the current implementation is known to be slow.
#'
#' @param nps numeric. Nominal point spacing (see reference page 533  section 2)
#' @param th numeric. Minimal height. Point below this threshold are not condisered for the segmentation
#' (see reference page 534 section 2)
#' @param MDCW numeric. Minimum detectable crown width (page 534 section 2)
#' @param epsilon numeric. Small deviation from vertical (page 535 section 2.2.2)
#' @param CLc numeric. Crown ratio of a narrow cone-shaped crown (page 535 equation 3)
#' @param Oc numeric. Crown radius reduction due to the overlap assuming the narrow cone-shaped tree
#' is situated in a dense stand (page 535 equation 3)
#' @param CLs numeric. Crown ratio of a sphere-shaped crown (page 535 equation 4)
#' @param Os numeric. Crown radius reduction due to the overlap within a dense stand for the sphere-shaped
#' tree (page 535 equation 4)
#' @param R numeric. Maximum horizontal distance of vertical profiles (page 535 sectioh 2.1). Any value
#' greater than a crown is good because this parameter does not affect the result. However, it greatly affects the
#' computation speed. The lower the value, the faster the method.
#' @param gap_sensitivity integer. In the original article, page 535 section 2.2.1, gaps are detected
#' using six times the interquartile range of square root distance between consecutive points. This
#' paramter control this value. Default is 6.
#' @param filter_profiles logical. This is an addition to the original method to filter dummy profiles
#' (see details)
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family point-cloud based tree segmentation algorithms
#'
#' @references
#' Hamraz, H., Contreras, M. A., & Zhang, J. (2016). A robust approach for tree segmentation in deciduous
#' forests using small-footprint airborne LiDAR data. International Journal of Applied Earth Observation
#' and Geoinformation, 52, 532–541. https://doi.org/10.1016/j.cageo.2017.02.017
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col =  pastel.colors(200)
#'
#' lastrees(las, hamraz2016())
#' plot(las, color = "treeID", colorPalette = pastel.colors(200))
#'}
hamraz2016 = function(nps = 0.25, th = 5, MDCW = 1.5, epsilon = 5, CLc = 0.8, Oc = 2/3, CLs = 0.7, Os = 1/3, gap_sensitivity = 6L, R = 15.24, filter_profiles = TRUE)
{
  assertive::assert_is_a_number(nps)
  assertive::assert_all_are_positive(nps)
  assertive::assert_is_a_number(th)
  assertive::assert_all_are_positive(th)
  assertive::assert_is_a_number(MDCW)
  assertive::assert_all_are_positive(MDCW)
  assertive::assert_is_a_number(epsilon)
  assertive::assert_all_are_positive(epsilon)
  assertive::assert_is_a_number(CLc)
  assertive::assert_all_are_positive(CLc)
  assertive::assert_is_a_number(Oc)
  assertive::assert_all_are_positive(Oc)
  assertive::assert_is_a_number(CLs)
  assertive::assert_all_are_positive(CLs)
  assertive::assert_is_a_number(Os)
  assertive::assert_all_are_positive(Os)
  assertive::assert_is_a_number(gap_sensitivity)
  assertive::assert_all_are_positive(gap_sensitivity)
  assertive::assert_is_a_number(R)
  assertive::assert_all_are_positive(R)
  assertive::assert_is_a_bool(filter_profiles)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lastrees"), "hamraz2016")

    . <- X <- Y <- Z <- NULL

    # Preprocess : LSP + remove low point + smooth
    LSP = LAS(las@data[, .(X,Y,Z)], las@header)
    LSP = lasfiltersurfacepoints(LSP, nps)    # page 533
    LSP = lasfilter(LSP, Z > th)              # page 534
    lassmooth(LSP, 3*nps, "gaussian", "square", sigma = nps)

    # ID initalization
    idTree = 0L
    treeID = rep(NA_integer_, nrow(las@data))

    # Progress estimation and stop criterion
    npts = nrow(LSP@data)
    npoints = nrow(LSP@data)
    pbar =  utils::txtProgressBar(0, npoints)

    while (npts != 0)
    {
      utils::setTxtProgressBar(pbar, npoints - npts)

      # (1) Locate the global maximum GMX (page 534)

      i = which.max(LSP@data$Z)
      GMX = LSP@data[i]
      GMX$i = i

      disc = lasclipCircle(LSP, GMX$X, GMX$Y, R)            # Extract a disc around GMX
      disc@data[, R := sqrt((X-GMX$X)^2 + (Y-GMX$Y)^2)]     # Compute cylindrical cordinates

      # (2-4) Find the convex hull according to Hamraz rules
      l = C_hamraz_segmentation(disc, nps, gap_sensitivity, MDCW, epsilon, CLc, CLs, Oc, Os, R)
      p = l$polygon
      data.table::setDT(p)

      #  Filter the convex hull and rebuild a new clean one
      if (filter_profiles)
      {
        p = p[p$R > 2 * nps]                          # Keep the profile over 1.5 m
        q = stats::quantile(p$R, probs = c(0.1, 0.9)) # Keep the profile within the 10 and 90th percentile of lenghts
        p = p[R < q[2] & R > q[1]]
      }

      area = 0
      if (nrow(p) > 3)
      {
        ch = convex_hull(p$X, p$Y)
        area = polygon_area(ch$x, ch$y)
      }


      # plot(disc@data$X, disc@data$Y, col = lidR:::set.colors(disc@data$Z, height.colors(50)), asp = 1)

      # x = numeric(64)
      # y = numeric(64)
      # a = numeric(64)
      # for(i in 1:64)
      # {
      #   x[i] = GMX$X + l$profile[[i]]$extremityPoint[5] * cos(l$profile[[i]]$angle*pi/180)
      #   y[i] = GMX$Y + l$profile[[i]]$extremityPoint[5] * sin(l$profile[[i]]$angle*pi/180)
      #   a[i] = l$profile[[i]]$angle
      #   points(l$profile[[i]]$extremityPoint[1], l$profile[[i]]$extremityPoint[2], col = "red", pch = 19)
      # }

      #lines(l$polygon[,1 ], l$polygon[,2], col = "red")
      #lines(p$x, p$y)

      # (5) cluster all LSPs encompassed within the convex hull and assign them as the current tallest tree crown
      in_p = logical(nrow(LSP@data))

      if(area == 0)
      {
        # The current point GMX will be removed (otherwise, we get stucked in a infinite loop).
        in_p[GMX$i] = TRUE
      }
      # else this is a normal case
      else
      {
        # Find the points that belong in the convex hull
        in_p = C_points_in_polygon(ch$x, ch$y, LSP@data$X, LSP@data$Y)

        # If no point found within this polygon only GMX will be remove
        if (sum(in_p) == 0) in_p[GMX$i] = TRUE
      }

      # extract the tree as a data.table
      tree = LSP@data[in_p]

      # extract the rest of the forest as a LAS
      LSP = suppressWarnings(lasfilter(LSP, !in_p))

      #plot(LSP@data$X, LSP@data$Y, col = lidR:::set.colors(LSP@data$Z, height.colors(50)), asp = 1)

      # There are still points to classify
      if (!is.null(LSP))
        npts = nrow(LSP@data)
      else
        npts = 0

      # Finally attribute an ID to each point of the original dataset (Hamraz considers only the LSP
      # but we classify the whole point cloud)
      if (area > pi*(MDCW/2)^2)
      {
        idTree <- idTree + 1L

        las_in_p = C_points_in_polygon(ch$x, ch$y, las@data$X, las@data$Y)

        # a new ID is attributed only to points that don't already have an ID (dominant has precedence)
        update = las_in_p & is.na(treeID)
        treeID[update] = idTree
      }

      #plot(las@data$X, las@data$Y, col = treeID, asp = 1)
    }

    return(treeID)
  }

  class(f) <- c("function", "PointCloudBased", "IndividualTreeSegmentation", "Algorithm", "lidR")

  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithms for tree
#' segmentation based on the Li et al. (2012) article (see reference). This method is a growing region
#' method working at the point cloud level. It is an implementation, as strict as possible, made by
#' the \code{lidR} author but with the addition of a parameter \code{hmin} to stop over-segmentation
#' for objects that are too low.
#'
#' @param dt1 numeric. Threshold number 1. See reference page 79 in Li et al. (2012). Default 1.5.
#'
#' @param dt2 numeric. Threshold number 2. See reference page 79 in Li et al. (2012). Default 2.
#'
#' @param R numeric. Search radius. See reference page 79 in Li et al. (2012). Default 2. If \code{R = 0}
#' all the points are automatically considered as local maxima and the search step is skipped (much
#' faster).
#'
#' @param hmin numeric.  Minimum height of a detected tree. Default 2.
#'
#' @param Zu numeric. If point elevation is greater than Zu, \code{dt2} is used, otherwise \code{dt1} is
#' used. See reference page 79 in Li et al. (2012). Default 15.
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
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' lastrees(las, li2012(dt1 = 1.4))
#' plot(las, color = "treeID", colorPalette = col)
li2012 = function(dt1 = 1.5, dt2 = 2, R = 2, Zu = 15, hmin = 2, speed_up = 10)
{
  assertive::assert_is_a_number(dt1)
  assertive::assert_is_a_number(dt2)
  assertive::assert_is_a_number(R)
  assertive::assert_is_a_number(Zu)
  assertive::assert_is_a_number(hmin)
  assertive::assert_is_a_number(speed_up)
  assertive::assert_all_are_positive(dt1)
  assertive::assert_all_are_positive(dt2)
  assertive::assert_all_are_non_negative(R)
  assertive::assert_all_are_positive(Zu)
  assertive::assert_all_are_positive(hmin)
  assertive::assert_all_are_positive(speed_up)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("lastrees"), "li2012")

    id = rep(NA_integer_, nrow(las@data))

    if (las@header@PHB$`Max Z` < hmin)
      warning("'hmin' is higher than the highest point. No tree segmented.")
    else
      id = C_lastrees_li2(las, dt1, dt2, Zu, R, hmin, speed_up)

    return(id)
  }

  class(f) <- c("function", "PointCloudBased", "IndividualTreeSegmentation", "Algorithm", "lidR")

  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithms for tree
#' segmentation based on the Silva et al. (2016) article (see reference). This is a simple method
#' based on seed + voronoi tesselation (equivalent to nearest neibourgh). This algorithm is implemented
#' in the package \code{rLiDAR}. This version is \emph{not} the version from \code{rLiDAR}. It is a
#' code written from the original article by the lidR authors and that is considerably (between 250
#' and 1000 times) faster.
#'
#' Because this algorithm works on a CHM only there is no actual need of a point cloud. Sometime the
#' user do not even have the point cloud that generated the CHM. \code{lidR} is a point cloud oriented
#' library this is why this algorithm must be used in \link{lastrees} to reafect the result into the point
#' cloud. However the user can use this function alone like that:
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
#' tree height. Default is 0.6,  meaning 60\% of the tree height.
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
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(200)
#'
#' chm = grid_canopy(las, res = 0.5, p2r(0.3))
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, lmf(4, 2))
#' lastrees(las, silva2016(chm, ttops))
#' plot(las, color = "treeID", colorPalette = col)
silva2016 = function(chm, treetops, max_cr_factor = 0.6, exclusion = 0.3, ID = "treeID")
{
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_all_of(treetops, "SpatialPointsDataFrame")
  assertive::assert_is_a_number(max_cr_factor)
  assertive::assert_is_a_number(exclusion)
  assertive::assert_all_are_in_open_range(max_cr_factor, 0, 1)
  assertive::assert_all_are_in_open_range(exclusion, 0, 1)

  f = function()
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})

    if (!is.null(context))
      stopif_wrong_context(context, c("lastrees"), "silva2016")

    . <- R <- X <- Y <- Z <- id <- d <- hmax <- NULL

    X = match_chm_and_seeds(chm, treetops, ID)
    cells = X$cells
    ids = X$ids

    chmdt = data.table::setDT(raster::as.data.frame(chm, xy = TRUE, na.rm = T))
    data.table::setnames(chmdt, names(chmdt), c("X", "Y", "Z"))

    # Voronoi tesselation is nothing else than the nearest neigbour
    u = C_knn(treetops@coords[,1], treetops@coords[,2], chmdt$X, chmdt$Y, 1L)
    chmdt[, id := u$nn.idx[,1]]
    chmdt[, id := ids[id]]
    chmdt[, d := u$nn.dist[,1]]

    chmdt[, hmax := max(Z), by = id]
    chmdt = chmdt[Z >= exclusion*hmax & d <= max_cr_factor*hmax, .(X,Y, id)]
    as.lasmetrics(chmdt, raster::res(chm)[1])
    crown = as.raster.lasmetrics(chmdt)

    return(crown)
  }

  class(f) <- c("function", "RasterBased", "IndividualTreeSegmentation", "Algorithm", "lidR")
  return(f)
}

#' Individual Tree Segmentation Algorithm
#'
#' This functions is made to be used in \link{lastrees}. It implements an algorithms for tree
#' segmentation based on a watershed or a marker controled watershed.
#' \itemize{
#' \item \strong{Simple watershed} is based on the bioconductor package \code{EBIimage}. You need to install
#' this package to run this method (see its \href{https://github.com/aoles/EBImage}{github page}).
#' Internally the function EBImage::watershed is called.
#' \item \strong{Marker controlled watershed} is based on the \code{imager} package. Internally the
#' function \link[imager:watershed]{imager::watershed} is called using the tree tops as priority map.
#' }
#'
#' Because this algorithm works on a CHM only there is no actual need of a point cloud. Sometime the
#' user do not even have the point cloud that generated the CHM. \code{lidR} is a point cloud oriented
#' library this is why this algorithm must be used in \link{lastrees} to reafect the result into the point
#' cloud. However the user can use this function alone like that:
#' \preformatted{
#'  chm = raster("file/to/a/chm/")
#'  ttops = tree_detection(chm, lmf(3))
#'  crowns = watershed(chm)()
#' }
#'
#' @template param-chm-lastrees
#'
#' @param th_tree numeric. Threshold below which a pixel cannot be a tree. Default 2.
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
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#' col = pastel.colors(250)
#'
#' chm = grid_canopy(las, res = 0.5, p2r(0.3))
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = mean, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, lmf(4, 2))
#' lastrees(las, mcwatershed(chm, ttops))
#'
#' plot(las, color = "treeID", colorPalette = col)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
watershed = function(chm, th_tree = 2, tol = 1, ext = 1)
{
  ws_generic(chm, th_tree = th_tree, tol = tol, ext = ext)
}

#' @rdname watershed
#' @export
mcwatershed = function(chm, treetops, th_tree = 2, ID = "treeID")
{
  ws_generic(chm, th_tree = th_tree, treetops = treetops, ID = ID)
}

ws_generic = function(chm, th_tree = 2, tol = 1, ext = 1, treetops = NULL, ID = "treeID")
{
  assertive::assert_is_all_of(chm, "RasterLayer")
  assertive::assert_is_a_number(th_tree)
  assertive::assert_is_a_number(tol)
  assertive::assert_is_a_number(ext)

  f = function()
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})

    if (!is.null(context))
      stopif_wrong_context(context, c("lastrees"), "watershed")

    # Test if requiered packages are installed
    if (is.null(treetops))
    {
      if (!requireNamespace("EBImage", quietly = TRUE))
        stop("'EBImage' package is needed for this function to work. Please read documentation.", call. = F)
    }
    else
    {
      if (!requireNamespace("imager", quietly = TRUE))
        stop("'imager' package is needed for this function to work.", call. = F)
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
    # Marker controlled watershed
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
