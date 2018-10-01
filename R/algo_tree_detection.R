# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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

# ===== LMF ======

#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{tree_detection}. It implements an algorithms for tree
#' detection based on a local maximum filter. The windows size can be fix or variable and the windows
#' shape can be square or circular. The internal algorithm works either with a raster or a point cloud.
#' It is deeply inspired from Popescu & Wynne (2004) (see references).
#'
#' @param ws numeric or function. Length or diameter of the moving window used to the detect the local
#' maxima in the unit of the input data (usually meters). If it is numeric a fixed windows size is used.
#' If it is a function, the function determines the size of the window at any given location on the canopy.
#' The function should take the height of a given pixel or points as its only argument and return the
#' desired size of the search window when centered on that pixel/point.
#'
#' @param hmin numeric. Minimum height of a tree. Threshold below which a pixel or a point
#' cannot be a local maxima. Default 2.
#'
#' @param shape character. Shape of the moving windows used to find the local maxima. Can be "square"
#' or "circular".
#'
#' @references
#' Popescu, Sorin & Wynne, Randolph. (2004). Seeing the Trees in the Forest: Using Lidar and
#' Multispectral Data Fusion with Local Filtering and Variable Window Size for Estimating Tree Height.
#' Photogrammetric Engineering and Remote Sensing. 70. 589-604. 10.14358/PERS.70.5.589.
#'
#' @export
#'
#' @family individual tree detection algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # point-cloud-based
#' # =================
#'
#' # 5x5 m fixed windows size
#' ttops = tree_detection(las, lmf(5))
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#'
#' # variable windows size
#' f = function(x) { x * 0.07 + 3}
#' ttops = tree_detection(las, lmf(f))
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#'
#' # raster-based
#' # ============
#'
#' # 5x5 m fixed windows size
#' chm = grid_canopy(las, res = 1, p2r(0.15))
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = median, na.rm = TRUE)
#'
#' ttops = tree_detection(chm, lmf(5))
#'
#' raster::plot(chm, col = height.colors(30))
#' sp::plot(ttops, add = TRUE)
#'
#' # variable windows size
#' f = function(x) { x * 0.07 + 3 }
#' ttops = tree_detection(chm, lmf(f))
#'
#' raster::plot(chm, col = height.colors(30))
#' sp::plot(ttops, add = TRUE)
lmf = function(ws, hmin = 2, shape = c("circular", "square"))
{
  shape = match.arg(shape)
  circular = shape == "circular"

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, "tree_detection", "lmf")

    n = nrow(las@data)

    if (assertive::is_a_number(ws))
    {
      # nothing to do
    }
    else if (assertive::is_function(ws))
    {
      ws = ws(las@data$Z)

      if (!is.numeric(ws)) stop("The function 'ws' did not return correct output. ", call. = FALSE)
      if (any(ws <= 0))    stop("The function 'ws' returned negative or nul values.", call. = FALSE)
      if (anyNA(ws))       stop("The function 'ws' returned NA values.", call. = FALSE)
      if (length(ws) != n) stop("The function 'ws' did not return correct output.", call. = FALSE)
    }
    else
      stop("'ws' must be a number or a function", call. = FALSE)

    . <- X <- Y <- Z <- treeID <- NULL
    is_maxima = C_LocalMaximumFilter(las@data, ws, hmin, circular)
    maxima = las@data[is_maxima, .(X,Y,Z)]
    maxima[, treeID := 1:.N]

    output = sp::SpatialPointsDataFrame(maxima[, .(X,Y)], maxima[, .(treeID, Z)])
    output@proj4string = las@proj4string
    output@bbox = sp::bbox(las)
    return(output)
  }

  class(f) <- c("PointCloudBased", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}

# ===== PTREE ======

#' Individual Tree Detection and Segmentation Algorithm
#'
#' This function is made to be used in \link{tree_detection} or \link{lastrees}. It implements the
#' PTrees algorithm for tree detection and tree segmentation based Vega et al. (2014) (see references).
#' When used in the function \link{tree_detection} it runs only the fisrt part of the method i.e. the
#' detection of the trees. When used in  \link{lastrees} it performs the  whole segmentation (see details).
#'
#' This function has been written by the \code{lidR} authors from the original article. We made our
#' best to implement as far as possible exactly what is written in the original paper but we cannot
#' states that it is the exact original algorithm. Also, minor variations were introduced to fix some
#' issues that were not adressed in the original paper:
#' \itemize{
#' \item Addition of the parameter \code{hmin}: to reduce oversegmentation we introduced a minium height
#' threshold. Point below this thresold cannot initiate new trees during the tree detection and cannot
#' incrase a crown hull during the segmentation.
#' \item Addition of the parameter \code{nmax}: in the original article page 103 figures 5, the number
#' of possible combination is 2^n-n-1. This exponential number of combinations lead, in some cases
#' to an infinite computation time. During the developpement we got cases where the number of combinations
#' to consider was beyond a billion. If the number of tree to consider between two scales is greater
#' than \code{nmax} (i.e. the number of combination is greater than 2^nmax-nmax-1) then the "TreeSegment"
#' from the biggest scale is retained anyway, the smallest scale is considered as dummy.
#' }
#' Notice that to use the PTree strictly as originally described, the point cloud should
#' not be normalized (see reference). In that case the parameter '\code{hmin}' is miningless and can
#' be set to \code{-Inf} for example.
#'
#' @param k integer vector. A serie of k-nearest neighbors to use. In this original paper a k refers
#' to a 'scale' of analyse (see reference).
#'
#' @param hmin scalar. This is an addition from the original paper to limit oversegmentation.
#' Point below this threshold cannot initiate new trees or increase a hull (see details). Set to \code{-Inf}
#' to strictly respect original paper.
#'
#' @param nmax integer. This is an addition from the original paper to protect against uncomputable
#' cases (see details). Set to \code{+Inf} to strictly respect the original paper (not recommended)
#'
#'@author Jasmin Siefert and Jean-Romain Roussel
#'
#' @export
#'
#' @family individual tree segmentation algorithms
#' @family individual tree detection algorithms
#' @family point-cloud based tree segmentation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' k = c(30,15)
#' ttops = tree_detection(las, ptrees(k))
#'
#' lastrees(las, ptrees(k))
ptrees = function(k, hmin = 2, nmax = 7L)
{
  assertive::assert_is_numeric(k)
  assertive::assert_all_are_positive(k)
  assertive::assert_all_are_whole_numbers(k)
  assertive::assert_is_a_number(nmax)
  assertive::assert_all_are_whole_numbers(nmax)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, c("tree_detection", "lastrees"), "ptrees")

    . <- X <- Y <- Z <- treeID <- NULL

    segmentation = context == "lastrees"

    TreeSegments = C_lastrees_ptrees(las, k, hmin, nmax, segmentation)

    if (!segmentation)
    {
      apices = TreeSegments$Apices
      apices = data.table::as.data.table(apices)
      data.table::setnames(apices, names(apices), c("X", "Y", "Z"))
      apices[, treeID := 1:.N]

      output = sp::SpatialPointsDataFrame(apices[, .(X,Y)], apices[, .(treeID, Z)])
      output@proj4string = las@proj4string
      output@bbox = sp::bbox(las)
      return(output)
    }
    else
    {
      return(TreeSegments$treeID)
    }
  }

  class(f) <- c("function", "PointCloudBased", "IndividualTreeSegmentation", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}

# ===== MANUAL ======

#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{tree_detection}. It implements an algorithm for manual
#' tree detection. User can point the tree top positions manually and interactively using the mouse.
#' This is only suitable for small size plots. First the point cloud is displayed, then the user is
#' invited to select a rectangular region of interest in the scene using the right button of the mouse.
#' Within the selected points the highest one will be flaged as 'tree top' in the scene. Once all the tree
#' are labelled the user can exit the tools by selecting an empty region. Points can also be unflagged.
#' The goal of this tool is mainly for minor correction of automatically detected tree outputs.
#'
#' @param detected \code{SpatialPointsDataFrame} or \code{data.table} or \code{data.frame} or \code{matrix}
#' containing X,Y,Z coordinates of already found tree tops that need manual corrections.
#'
#' @param ... supplementary parameters to be pass to \link{plot}.
#'
#' @family individual tree detection algorithms
#'
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # Full manual tree finding
#' ttops = tree_detection(las, manual())
#'
#' # Automatic finding with manual correction
#' ttops = tree_detection(las, lmf(5))
#' ttops = tree_detection(las, manual(ttops))
#' }
manual = function(detected = NULL, ...)
{
  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, "tree_detection", "manual")

    . <- X <- Y <-Z <- treeID <- NULL

    stopifnotlas(las)
    crs = sp::CRS()

    if (!interactive())
      stop("R is not being used interactively", call. = FALSE)

    if (is.null(detected))
      apice <- data.table::data.table(X = numeric(0), Y = numeric(0), Z = numeric(0))
    else if (is(detected, "SpatialPointsDataFrame"))
    {
      crs = detected@proj4string
      apice <- data.table::data.table(detected@coords)
      apice$Z = detected@data$Z
      names(apice) <- c("X","Y","Z")
    }
    else
    {
      apice <- data.table::as.data.table(detected[,1:3])
      names(apice) <- c("X","Y","Z")
    }

    plot.LAS(las, ...)

    id = numeric(nrow(apice))
    for (i in 1:nrow(apice))
      id[i] = rgl::spheres3d(apice$X[i], apice$Y[i], apice$Z[i], radius = 1, color = "red")
    apice$id = id

    repeat
    {
      f <- rgl::select3d(button = c("right"))
      pts <- las@data[f(las@data), .(X,Y,Z)]

      if (length(pts$X) == 0)
        break;

      apex <- unique(pts[pts$Z == max(pts$Z)])

      ii = which(apice$X == apex$X & apice$Y == apex$Y & apice$Z == apex$Z)

      if (length(ii) > 0)
      {
        rgl::rgl.pop(id = apice[ii]$id)
        apice = apice[-ii]
      }
      else
      {
        apex$id = as.numeric(rgl::spheres3d(apex$X, apex$Y, apex$Z, radius = 1, color = "red"))
        apice = rbind(apice, apex)
      }
    }

    rgl::rgl.close()

    apice[, treeID := 1:.N]
    output = sp::SpatialPointsDataFrame(apice[, .(X,Y)], apice[, .(treeID, Z)], proj4string = crs)
    return(output)
  }


  class(f) <- c("function", "PointCloudBased", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}

# ===== MULTICHM ======


#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{tree_detection}. It implements an algorithms for tree
#' detection based on a method described in Eysn et al (2015) (see references) and propably proposed
#' originaly by someone else (we did not find original publication). This is a local maximum filter
#' applied on a multi-canopy height model (see details).\cr\cr
#' Notice: tree tops returned are the true highest points within a given pixel whenever the CHMs where
#' computed with the 95th percentile of height. Otherwise these maxima are not true maxima and cannot
#' be used in subsequent segmentation algorithms.
#'
#' Describtion adapted from Eysn et al (2015), page 1728, section 3.1.3 Method #3\cr\cr
#' The method is based on iterative canopy height model generation (CHM) and local maximum filter (LMF)
#' detection within a moving window for various CHMs. The method works in two general steps, which are
#' (a) sequential identification of potential trees and (b) filtering of the extracted potential trees.\cr
#' \itemize{
#' \item Step (a): From the normalized point cloud, an initial CHM is created by assigning the 95th
#' height percentile within each raster cell. Based on this CHM, LM are detected and the found
#' positions and heights are stored in a database. For the next iteration, points in the uppermost layer
#' of the normalized ALS data are eliminated. The “eliminating” layer is defined as a band below the
#' current CHM. Based on the filtered data, a new CHM is created, LM are extracted, and the
#' LM parameters are added to the database. This procedure is carried out sequentially until all points
#' are removed from the normalized point cloud.
#' \item Step (b): All detected LM in the database are sorted by decreasing heights.
#' The highest LM is considered a detected tree. For each following LM, the LM is considered a
#' detected tree if there is no detected tree within a given 2D distance as well as a given 3D distance.
#' }

#' @param res numeric. Resolution of the CHM based on the 95th percentile
#'
#' @param layer_thickness numeric. The “eliminating” layer is defined as a band of \code{layer_thickness} m
#' below the current CHM (see details).
#'
#' @param dist_2d numeric. 2D distance threshold. A local maximum is considered a detected tree
#' if there is no detected tree within this 2D distance (see details).
#'
#' @param dist_3d numeric. 3D distance threshold. A local maximum is considered a detected tree
#' if there is no detected tree within this 3D distance (see details).
#'
#' @param use_max logical. The CHMs are computed with the 95th percentiles of height in the original
#' description. If \code{use_max = TRUE} it uses the 100th percentiles (max height) and thus does
#' not implies any sorting algorithm. The algoithm is therefore 5 to 10 times faster.
#'
#' @param ... supplementary parameters to be pass to \link{lmf} that is used internally
#' to find the local maxima.
#'
#' @export
#'
#' @references
#' Eysn, L., Hollaus, M., Lindberg, E., Berger, F., Monnet, J. M., Dalponte, M., … Pfeifer, N. (2015).
#' A benchmark of lidar-based single tree detection methods using heterogeneous forest data from the
#' Alpine Space. Forests, 6(5), 1721–1747. https://doi.org/10.3390/f6051721
#'
#' @family individual tree detection algorithms
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' ttops = tree_detection(las, multichm(res = 1, ws = 5))
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#' }
multichm = function(res = 1, layer_thickness = 0.5, dist_2d = 3, dist_3d = 5, use_max = FALSE, ...)
{
  assertive::assert_is_a_number(res)
  assertive::assert_is_a_number(layer_thickness)
  assertive::assert_is_a_number(dist_2d)
  assertive::assert_is_a_number(dist_3d)
  assertive::assert_all_are_positive(res)
  assertive::assert_all_are_positive(layer_thickness)
  assertive::assert_all_are_positive(dist_2d)
  assertive::assert_all_are_positive(dist_3d)

  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, "tree_detection", "multichm")

    . <- X <- Y <- Z <- treeID <- NULL

    dist_2d <- dist_2d^2
    dist_3d <- dist_3d^2

    las_copy <- LAS(las@data[, .(X,Y,Z)], las@header)
    LM       <- list()
    chm      <- grid_canopy(las, res, p2r())
    i        <- 1
    p        <- list(...)
    hmin     <- if(is.null(p$hmin)) formals(lmf)$hmin else p$hmin

    while(!is.empty(las_copy))
    {
      if (use_max)
        chm95 <- grid_canopy(las_copy, res, p2r())
      else
        chm95 <- grid_metrics(las_copy, stats::quantile(Z, probs = 0.95), res)

      if (max(chm95[], na.rm = TRUE) > hmin)
      {
        lm       <- tree_detection(chm95, lmf(...))
        lm       <- raster::as.data.frame(lm)
        data.table::setDT(lm)
        LM[[i]]  <- lm
        las_copy <- lasclassify(las_copy, chm95, "chm95")
        las_copy <- lasfilter(las_copy, Z < chm95 - layer_thickness)

        i <- i+1
      }
      else
        las_copy <- new("LAS")
    }

    LM <- data.table::rbindlist(LM)
    data.table::setorder(LM, -Z)
    LM <- unique(LM, by = c("X", "Y"))

    detected = logical(nrow(LM))
    detected[1] = TRUE

    for (i in 2:nrow(LM))
    {
      distance2D = (LM$X[i] - LM$X[detected])^2 + (LM$Y[i] - LM$Y[detected])^2
      distance3D = distance2D + (LM$Z[i] - LM$Z[detected])^2

      if (!any(distance2D < dist_2d) & !any(distance3D < dist_3d))
      {
        detected[i] = TRUE
      }
    }

    detected = LM[detected]
    detected[, treeID := 1:.N]

    output <- sp::SpatialPointsDataFrame(detected[, 1:2], detected[, 3:4], proj4string = las@proj4string)
    return(output)
  }

  class(f) <- c("function", "PointCloudBased", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}


# ===== LMFX ======

lmfx = function(ws, hmin = 2, dist_2d = 3, dist_3d = 5)
{
  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    lidR:::stopif_wrong_context(context, "tree_detection", "lmf")

    . <- X <- Y <- Z <- treeID <- NULL

    dist_2d = dist_2d^2
    dist_3d = dist_3d^2

    if (assertive::is_a_number(ws))
    {
      # nothing to do
    }
    else if (assertive::is_function(ws))
    {
      n  = nrow(las@data)
      ws = ws(las@data$Z)

      if (!is.numeric(ws)) stop("The function 'ws' did not return correct output. ", call. = FALSE)
      if (any(ws <= 0))    stop("The function 'ws' returned negative or nul values.", call. = FALSE)
      if (anyNA(ws))       stop("The function 'ws' returned NA values.", call. = FALSE)
      if (length(ws) != n) stop("The function 'ws' did not return correct output.", call. = FALSE)
    }
    else
      stop("'ws' must be a number or a function", call. = FALSE)

    . <- X <- Y <- Z <- treeID <- NULL
    is_maxima = lidR:::C_LocalMaximumFilter(las@data, ws, hmin, TRUE)
    LM = las@data[is_maxima, .(X,Y,Z)]

    data.table::setorder(LM, -Z)

    detected = LM[1,.(X,Y,Z)]

    knn = with(detected, lidR:::C_knn(X,Y,X,Y,2))

    for (i in 2:nrow(LM))
    {
      distance2D = (LM$X[i] - detected$X)^2 + (LM$Y[i] - detected$Y)^2
      distance3D = distance2D + (LM$Z[i] - detected$Z)^2

      if (!any(distance2D < dist_2d) & !any(distance3D < dist_3d))
        detected = rbind(detected, data.table(X = LM$X[i], Y = LM$Y[i], Z = LM$X[i]))
    }

    detected[, treeID := 1:.N]

    output = sp::SpatialPointsDataFrame(detected[, .(X,Y)], detected[, .(treeID, Z)])
    output@proj4string = las@proj4string
    output@bbox = sp::bbox(las)
    return(output)
  }

  class(f) <- c("PointCloudBased", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}
