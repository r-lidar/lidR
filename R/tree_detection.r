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

#' Tree top detection
#'
#' This function is a wrapper for all the implemented tree detection methods. Tree detection functions
#' find the position of the trees before the segmentation process. Several methods may be used
#' (see documentation of each method)
#'
#' @template LAScatalog
#' @template section-supported-option-tree_detection
#'
#' @param x An object of class \code{LAS} or \code{LAScatalog} or a \code{RasterLayer} representing a
#' canopy height model. depending on the algorithm
#' used (see respective documentation)
#' @param algorithm character. Can be either \code{"lmf"} or \code{"ptrees"}.
#' @param ... Other parameters for each respective algorithm (see section "see also").
#'
#' @return The output of each algorithm as documedevtools::chented in each method.
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' ttops = tree_detection(las, "lmf", ws = 5)
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#' @seealso \link{tree_detection_lmf} \link{tree_detection_ptrees} \link{tree_detection_manual} \link{tree_detection_multichm}
tree_detection = function(x, algorithm, ...)
{
  if (algorithm == "lmf")
    tree_detection_lmf(x, ...)
  else if (algorithm == "ptrees")
    tree_detection_ptrees(x, ...)
  else if(algorithm == "manual")
    tree_detection_manual(x, ...)
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

#' Tree top detection based local maxima filters
#'
#' Tree top detection based on local maxima filters. The windows size can be fix or variable and the
#' windows shape can be square or circular. The internal algorithm works either with a raster or a point
#' cloud
#'
#' @template LAScatalog
#' @template section-supported-option-tree_detection
#'
#' @param x An object of class \code{LAS} or \code{LAScatalog} or a \code{RasterLayer} representing a
#' canopy height model.
#' @param ws numeric or function. Length or diameter of the moving window used to the detect the local
#' maxima in the unit of the input data (usually meters). If it is numeric a fixed windows size is used.
#' If it is a function, the function determines the size of the window at any given location on the canopy.
#' It should take the height of a given pixel or points as its only argument and return the desired size
#' of the search window when centered on that pixel/point.
#' @param hmin numeric. Minimum height of a tree. Threshold below which a pixel or a point
#' cannot be a local maxima. Default 2.
#' @param shape character. Shape of the moving windows used to find the local maxima. Can be "square"
#' or "circular".
#' @param ... Additional argument for \link{readLAS} to reduce the amount of data loaded (only with a
#' \code{LAScatalog} object).  Actually \code{filter} is the sole authorized argument since \code{select = "xyz"}
#' is imposed internally.
#'
#' @template return-tree_detection
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz", filter = "-drop_z_below 0")
#'
#' # point-cloud-based
#' # =================
#'
#' # 5x5 m fixed windows size
#' ttops = tree_detection_lmf(las, 5)
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#'
#' # variable windows size
#' f = function(x) { x * 0.07 + 3}
#' ttops = tree_detection_lmf(las, f)
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#'
#' # raster-based
#' # ============
#'
#' # 5x5 m fixed windows size
#' chm = grid_canopy(las, "p2r", 1, subcircle = 0.15)
#' kernel = matrix(1,3,3)
#' chm = raster::focal(chm, w = kernel, fun = median, na.rm = TRUE)
#'
#' ttops = tree_detection_lmf(chm, 5)
#'
#' raster::plot(chm, col = height.colors(30))
#' sp::plot(ttops, add = TRUE)
#'
#' # variable windows size
#' f = function(x) { x * 0.07 + 3 }
#' ttops = tree_detection_lmf(chm, f)
#'
#' raster::plot(chm, col = height.colors(30))
#' sp::plot(ttops, add = TRUE)
tree_detection_lmf = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  assertive::assert_is_a_number(hmin)
  assertive::assert_all_are_non_negative(hmin)

  UseMethod("tree_detection_lmf", x)
}

#' @export
tree_detection_lmf.RasterLayer = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  y = raster::as.data.frame(x, xy = T, na.rm = T)
  data.table::setDT(y)
  data.table::setnames(y, names(y), c("X", "Y", "Z"))

  output = tree_detection_lmf.data.frame(y , ws, hmin, shape)
  output@proj4string = x@crs
  output@bbox = raster::as.matrix(raster::extent(x))
  return(output)
}

#' @export
tree_detection_lmf.LAS = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  output = tree_detection_lmf(x@data , ws, hmin, shape)
  output@proj4string = x@proj4string
  output@bbox = sp::bbox(x)
  return(output)
}

#' @export
tree_detection_lmf.data.frame = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  shape = match.arg(shape)
  circular = shape == "circular"
  n = nrow(x)

  if (assertive::is_a_number(ws))
  {
    # nothing to do
  }
  else if (assertive::is_function(ws))
  {
    ws = ws(x$Z)

    if (!is.numeric(ws)) stop("The function 'ws' did not return correct output. ", call. = FALSE)
    if (any(ws <= 0))    stop("The function 'ws' returned negative or nul values.", call. = FALSE)
    if (anyNA(ws))       stop("The function 'ws' returned NA values.", call. = FALSE)
    if (length(ws) != n) stop("The function 'ws' did not return correct output.", call. = FALSE)
  }
  else
    stop("'ws' must be a number or a function", call. = FALSE)

  . <- X <- Y <- Z <- treeID <- NULL
  is_maxima = C_LocalMaximumFilter(x, ws, hmin, circular)
  maxima = x[is_maxima, .(X,Y,Z)]
  maxima[, treeID := 1:.N]

  output = sp::SpatialPointsDataFrame(maxima[, .(X,Y)], maxima[, .(treeID, Z)])
  return(output)
}

#' @export
tree_detection_lmf.LAScluster = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  las <- readLAS(x, ...)
  if (is.null(las)) return(NULL)
  ttops <- tree_detection_lmf(las, ws, hmin, shape)
  bbox  <- raster::extent(x)
  ttops <- raster::crop(ttops, bbox)
  return(ttops)
}

#' @export
tree_detection_lmf.LAScatalog = function(x, ws, hmin = 2, shape = c("circular", "square"), ...)
{
  x@input_options$select <- "xyz"

  output <- catalog_apply2(x, tree_detection_lmf, ws = ws, hmin = hmin, shape = shape, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE)
  output <- do.call(rbind, output)
  output@proj4string <- x@proj4string
  output@data$treeID <- 1:length(output@data$treeID)
  return(output)
}

#' @rdname lastrees_ptrees
#' @aliases tree_detection_ptrees
#' @export
tree_detection_ptrees = function(las, k, hmin = 3, nmax = 7L, ...)
{
  assertive::assert_is_numeric(k)
  assertive::assert_all_are_positive(k)
  assertive::assert_all_are_whole_numbers(k)
  assertive::assert_is_a_number(nmax)
  assertive::assert_all_are_whole_numbers(nmax)

  UseMethod("tree_detection_ptrees", las)
}

#' @export
tree_detection_ptrees.LAS = function(las, k, hmin = 3, nmax = 7L, ...)
{
  . <- X <- Y <- Z <- treeID <- NULL

  TreeSegments = C_lastrees_ptrees(las, k, hmin, nmax, FALSE)
  apices = TreeSegments$Apices
  apices = data.table::as.data.table(apices)
  data.table::setnames(apices, names(apices), c("X", "Y", "Z"))
  apices[, treeID := 1:.N]

  output = sp::SpatialPointsDataFrame(apices[, .(X,Y)], apices[, .(treeID, Z)])
  output@proj4string = las@proj4string
  return(output)
}

#' @export
tree_detection_ptrees.LAScluster = function(las, k, hmin = 3, nmax = 7L, ...)
{
  x <- readLAS(las, ...)
  if (is.empty(x)) return(NULL)
  ttops <- tree_detection_ptrees(x, k, hmin, nmax)
  bbox  <- raster::extent(las)
  ttops <- raster::crop(ttops, bbox)
  return(ttops)
}

#' @export
tree_detection_ptrees.LAScatalog = function(las, k, hmin = 3, nmax = 7L, ...)
{
  set_select(las) <- "xyz"

  output <- catalog_apply2(las, tree_detection_ptrees, k = k, hmin = hmin, nmax = nmax, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE)
  output <- do.call(rbind, output)
  output@proj4string <- las@proj4string
  output@data$treeID <- 1:length(output@data$treeID)
  return(output)
}

#' Tree top detection based on manual selection
#'
#' Find the tree top positions manually and interactively using the mouse. This is only suitable for
#' small to medium size plots. First the point cloud is displayed, then the user is invited to select
#' a rectangular region of interest in the scene using the right button of the mouse. Within the selected
#' points the highest will be flaged as 'tree top' in the scene. Once all the tree are labelled the user can
#' exit the tools by selecting an empty region. Points can also be unflagged.
#'
#' @param las An object of the class LAS
#' @param detected \code{SpatialPointsDataFrame} or \code{data.table} or \code{data.frame} or \code{matrix}
#' containing X,Y,Z coordinates of already found tree tops that need manual corrections.
#' @param ... supplementary parameters to be pass to \link{plot}.
#'
#' @template return-tree_detection
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # Full manual tree finding
#' ttops = tree_detection_manual(las)
#'
#' # Automatic finding with manual correction
#' ttops = tree_detection_lmf(las, 5)
#' ttops = tree_detection_manual(las, ttops)
#' }
tree_detection_manual = function(las, detected = NULL, ...)
{
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


#' Tree top detection based on LMF and multi-CHM
#'
#' Find the tree tops positions based on a method described in Eysn et al (2015) (see references) and
#' propably proposed originaly by Milan Kobal (we did not find original publication). This is a local
#' maximum filter applied on a multi-canopy height model (see details). The tree tops returned are the
#' true highest points within a given pixel whenever the CHMs where computed with the 95th percentile
#' of height. Otherwise these maxima are not true maxima and cannot be used in subsequent segmentation
#' algorithms.
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
#' @param las An object of the class LAS
#' @param res numeric. Resolution of the CHM based on the 95th percentile
#' @param layer_thickness numeric. The “eliminating” layer is defined as a band of \code{layer_thickness} m
#' below the current CHM (see details).
#' @param dist_2d numeric. 2D distance threshold. A local maximum is considered a detected tree
#' if there is no detected tree within this 2D distance (see details).
#' @param dist_3d numeric. 3D distance threshold. A local maximum is considered a detected tree
#' if there is no detected tree within this 3D distance (see details).
#' @param ... supplementary parameters to be pass to \link{tree_detection_lmf} that is used internally
#' to find the local maxima.
#'
#' @template return-tree_detection
#' @export
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' ttops = tree_detection_multichm(las, 1, ws = 5)
#'
#' plot(las)
#' rgl::spheres3d(ttops@coords[,1], ttops@coords[,2], ttops@data$Z, col = "red", size = 5, add = TRUE)
#' }
#' @references
#' Eysn, L., Hollaus, M., Lindberg, E., Berger, F., Monnet, J. M., Dalponte, M., … Pfeifer, N. (2015).
#' A benchmark of lidar-based single tree detection methods using heterogeneous forest data from the
#' Alpine Space. Forests, 6(5), 1721–1747. https://doi.org/10.3390/f6051721
tree_detection_multichm = function(las, res, layer_thickness = 0.5, dist_2d = 3, dist_3d = 5, ...)
{
  assertive::assert_is_a_number(res)
  assertive::assert_is_a_number(layer_thickness)
  assertive::assert_is_a_number(dist_2d)
  assertive::assert_is_a_number(dist_3d)
  assertive::assert_all_are_positive(res)
  assertive::assert_all_are_positive(layer_thickness)
  assertive::assert_all_are_positive(dist_2d)
  assertive::assert_all_are_positive(dist_3d)

  UseMethod("tree_detection_multichm", las)
}

#' @export
tree_detection_multichm.LAS = function(las, res, layer_thickness = 0.5, dist_2d = 3, dist_3d = 5, ...)
{
  . <- X <- Y <- Z <- treeID <- NULL

  dist_2d = dist_2d^2
  dist_3d = dist_3d^2

  las_copy = LAS(las@data[, .(X,Y,Z)], las@header)
  LM = list()
  chm = grid_metrics(las, max(Z), res)
  i = 1

  p = list(...)
  hmin = if(is.null(p$hmin)) formals(tree_detection_lmf)$hmin else p$hmin


  while(!is.empty(las_copy))
  {
    chm95 = grid_metrics(las_copy, stats::quantile(Z, probs = 0.95), res)

    if (max(chm95[], na.rm = TRUE) > hmin)
    {
      lm  = tree_detection_lmf(chm95, 4, ...)
      lm  = raster::as.data.frame(lm)
      data.table::setDT(lm)
      LM[[i]] = lm
      lasclassify(las_copy, chm95, "chm95")
      las_copy = lasfilter(las_copy, Z < chm95 - layer_thickness)
      i = i+1
    }
    else
      las_copy = new("LAS")
  }

  LM = data.table::rbindlist(LM)
  data.table::setorder(LM, -Z)

  detected = LM[1]
  for(i in 2:nrow(LM))
  {
    lm = LM[i]
    distance2D = (lm$X - detected$X)^2 + (lm$Y - detected$Y)^2
    distance3D = distance2D + (lm$Z - detected$Z)^2

    if (!any(distance2D < dist_2d) & !any(distance3D < dist_3d))
      detected = rbind(detected, lm)
  }

  detected[, treeID := 1:.N]

  output = sp::SpatialPointsDataFrame(detected[, .(X,Y)], detected[, .(treeID, Z)], proj4string = las@proj4string)
  return(output)
}

#' @export
tree_detection_multichm.LAScluster = function(las, res, layer_thickness = 0.5, dist_2d = 3, dist_3d = 5, ...)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  ttops <- tree_detection_multichm(x, res, layer_thickness, dist_2d, dist_3d)
  bbox  <- raster::extent(las)
  ttops <- raster::crop(ttops, bbox)
  return(ttops)
}

#' @export
tree_detection_multichm.LAScatalog = function(las, res, layer_thickness = 0.5, dist_2d = 3, dist_3d = 5, ...)
{
  set_select(las) <- "xyz"

  output <- catalog_apply2(las, tree_detection_multichm, res = res, layer_thickness = layer_thickness, dist_2d = dist_2d, dist_3d = dist_3d, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE)
  output <- do.call(rbind, output)
  output@proj4string <- las@proj4string
  output@data$treeID <- 1:length(output@data$treeID)
  return(output)
}

