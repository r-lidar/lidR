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
#' This function is made to be used in \link{tree_detection}. It implements an algorithm for tree
#' detection based on a local maximum filter. The windows size can be fixed or variable and its
#' shape can be square or circular. The internal algorithm works either with a raster or a point cloud.
#' It is deeply inspired from Popescu & Wynne (2004) (see references).
#'
#' @param ws numeric or function. Length or diameter of the moving window used to the detect the local
#' maxima in the unit of the input data (usually meters). If it is numeric a fixed window size is used.
#' If it is a function, the function determines the size of the window at any given location on the canopy.
#' The function should take the height of a given pixel or points as its only argument and return the
#' desired size of the search window when centered on that pixel/point.
#'
#' @param hmin numeric. Minimum height of a tree. Threshold below which a pixel or a point
#' cannot be a local maxima. Default is 2.
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
#' x = plot(las)
#' add_treetops3d(x, ttops)
#'
#' # variable windows size
#' f = function(x) { x * 0.07 + 3}
#' ttops = tree_detection(las, lmf(f))
#'
#' x = plot(las)
#' add_treetops3d(x, ttops)
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

    if (is.numeric(ws))
    {
      # nothing to do
    }
    else if (is.function(ws))
    {
      ws = ws(las@data$Z)
      ws[las@data$Z < hmin] = ws(hmin)

      if (!is.numeric(ws)) stop("The function 'ws' did not return a correct output. ", call. = FALSE)
      if (any(ws <= 0))    stop("The function 'ws' returned negative or null values.", call. = FALSE)
      if (anyNA(ws))       stop("The function 'ws' returned NA values.", call. = FALSE)
      if (length(ws) != n) stop("The function 'ws' did not return a correct output.", call. = FALSE)
    }
    else
      stop("'ws' must be a number or a function", call. = FALSE)

    . <- X <- Y <- Z <- treeID <- NULL
    is_maxima = C_lmf(las@data, ws, hmin, circular)
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

# ===== MANUAL ======

#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{tree_detection}. It implements an algorithm for manual
#' tree detection. Users can pinpoint the tree top positions manually and interactively using the mouse.
#' This is only suitable for small-sized plots. First the point cloud is displayed, then the user is
#' invited to select a rectangular region of interest in the scene using the right mouse button.
#' Within the selected points the highest one will be flagged as 'tree top' in the scene. Once all the trees
#' are labeled the user can exit the tool by selecting an empty region. Points can also be unflagged.
#' The goal of this tool is mainly for minor correction of automatically-detected tree outputs.
#'
#' @param detected \code{SpatialPointsDataFrame} of already found tree tops that need manual corrections.
#' @param radius numeric. Radius of the spheres displayed on the point cloud (aesthetic purpose only).
#' @param color character. Color of the spheres displayed on the point cloud (aesthetic purpose only).
#' @param ... supplementary parameters to be passed to \link{plot}.
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
manual = function(detected = NULL, radius = 0.5, color = "red", ...)
{
  f = function(las)
  {
    context <- tryCatch({get("lidR.context", envir = parent.frame())}, error = function(e) {return(NULL)})
    stopif_wrong_context(context, "tree_detection", "manual")

    . <- X <- Y <- Z <- treeID <- NULL

    stopifnotlas(las)
    crs = sp::CRS()

    if (!interactive())
      stop("R is not being used interactively", call. = FALSE)

    if (is.null(detected))
    {
      apice <- data.table::data.table(X = numeric(0), Y = numeric(0), Z = numeric(0))
    }
    else if (is(detected, "SpatialPointsDataFrame"))
    {
      crs          <- detected@proj4string
      apice        <- data.table::data.table(detected@coords)
      apice$Z      <- detected@data[["Z"]]
      names(apice) <- c("X","Y","Z")
    }
    else
    {
      stop("Input is not of the good type.")
    }

    minx <- min(las$X)
    miny <- min(las$Y)

    las@data <- las@data[, .(X, Y, Z)]
    las@data[, X := X - minx]
    las@data[, Y := Y - miny]
    apice[, X := X - minx]
    apice[, Y := Y - miny]

    plot.LAS(las, ..., clear_artifacts = FALSE)

    id = numeric(nrow(apice))

    for (i in 1:nrow(apice))
      id[i] = rgl::spheres3d(apice$X[i], apice$Y[i], apice$Z[i], radius = radius, color = color)

    apice$id <- id

    repeat
    {
      f <- rgl::select3d(button = c("right"))

      i <- f(apice)

      if (sum(i) > 0)
      {
        ii <- which(i == TRUE)[1]
        rgl::rgl.pop(id = apice[ii]$id)
        apice <- apice[-ii]
      }
      else
      {
        i <- f(las@data)

        if (sum(i) == 0)
          break;

        pts     <- las@data[i, .(X,Y,Z)]
        apex    <- unique(pts[pts$Z == max(pts$Z)])[1]
        ii      <- which(apice$X == apex$X & apice$Y == apex$Y & apice$Z == apex$Z)
        apex$id <- as.numeric(rgl::spheres3d(apex$X, apex$Y, apex$Z, radius = radius, color = color))
        apice   <- rbind(apice, apex)
      }
    }

    rgl::rgl.close()

    apice[, treeID := 1:.N]
    apice[, X := X + minx]
    apice[, Y := Y + miny]
    output <- sp::SpatialPointsDataFrame(apice[, .(X,Y)], apice[, .(treeID, Z)], proj4string = crs)
    return(output)
  }


  class(f) <- c("function", "PointCloudBased", "IndividualTreeDetection", "Algorithm", "lidR")
  return(f)
}
