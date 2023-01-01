# ===== LMF ======

#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{locate_trees}. It implements an algorithm for tree
#' detection based on a local maximum filter. The windows size can be fixed or variable and its
#' shape can be square or circular. The internal algorithm works either with a raster or a point cloud.
#' It is deeply inspired by Popescu & Wynne (2004) (see references).
#'
#' @param ws numeric or function. Length or diameter of the moving window used to detect the local
#' maxima in the units of the input data (usually meters). If it is numeric a fixed window size is used.
#' If it is a function, the function determines the size of the window at any given location on the canopy.
#' By default function takes the height of a given pixel or point as its only argument and return the
#' desired size of the search window when centered on that pixel/point. This can be controled with
#' the `ws_args` parameter
#' @param hmin numeric. Minimum height of a tree. Threshold below which a pixel or a point
#' cannot be a local maxima. Default is 2.
#' @param shape character. Shape of the moving window used to find the local maxima. Can be "square"
#' or "circular".
#' @param ws_args list. Named list of argument for the function `ws` if `ws` is a function.
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
#' las <- readLAS(LASfile, select = "xyzi", filter = "-inside 481250 3812980 481300 3813050")
#'
#' # =================
#' # point-cloud-based
#' # =================
#'
#' # 5x5 m fixed window size
#' ttops <- locate_trees(las, lmf(5))
#'
#' #plot(las) |> add_treetops3d(ttops)
#'
#' # variable windows size
#' f <- function(x) { x * 0.07 + 3}
#' ttops <- locate_trees(las, lmf(f))
#'
#' #plot(las) |> add_treetops3d(ttops)
#'
#' # Very custom variable windows size
#' f <- function(x, y, z) { x * 0.07 + y * 0.01 + z}
#' ws_args <- list(x = "Z", y = "Intensity", z = 3)
#' ttops <- locate_trees(las, lmf(f, ws_args = ws_args))
#'
#' # ============
#' # raster-based
#' # ============
#'
#' chm <- rasterize_canopy(las, res = 1, p2r(0.15), pkg = "terra")
#' ttops <- locate_trees(chm, lmf(5))
#'
#' plot(chm, col = height.colors(30))
#' plot(sf::st_geometry(ttops), add = TRUE, col = "black", cex = 0.5, pch = 3)
#'
#' # variable window size
#' f <- function(x) { x * 0.07 + 3 }
#' ttops <- locate_trees(chm, lmf(f))
#'
#' plot(chm, col = height.colors(30))
#' plot(sf::st_geometry(ttops), add = TRUE, col = "black", cex = 0.5, pch = 3)
#' @name itd_lmf
lmf = function(ws, hmin = 2, shape = c("circular", "square"), ws_args = "Z")
{
  shape <- match.arg(shape)
  circ  <- shape == "circular"
  ws    <- lazyeval::uq(ws)
  hmin  <- lazyeval::uq(hmin)
  ws_args  <- lazyeval::uq(ws_args)

  if (!is.numeric(ws) & !is.function(ws))
    stop("'ws' must be a number or a function", call. = FALSE)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTITD, "lmf")

    if (is.function(ws))
    {
      args <- lapply(ws_args, function(x) if (x %in% names(las)) las@data[[x]] else x)
      ws <- do.call(ws, args)
      b <- las$Z < hmin
      ws[b] <- min(ws)

      n <- npoints(las)
      if (!is.numeric(ws)) stop("The function 'ws' did not return a correct output. ", call. = FALSE)
      if (any(ws <= 0))    stop("The function 'ws' returned negative or null values.", call. = FALSE)
      if (anyNA(ws))       stop("The function 'ws' returned NA values.",               call. = FALSE)
      if (length(ws) != n) stop("The function 'ws' did not return a correct output.",  call. = FALSE)
    }

    force_autoindex(las) <- LIDRGRIDPARTITION
    return(C_lmf(las, ws, hmin, circ, getThread()))
  }

  f <- plugin_itd(f, omp = TRUE, raster_based = FALSE)
  return(f)
}

# ===== MANUAL ======

#' Individual Tree Detection Algorithm
#'
#' This function is made to be used in \link{locate_trees}. It implements an algorithm for manual
#' tree detection. Users can pinpoint the tree top positions manually and interactively using the mouse.
#' This is only suitable for small-sized plots. First the point cloud is displayed, then the user is
#' invited to select a rectangular region of interest in the scene using the mouse button.
#' Within the selected region the highest point will be flagged as 'tree top' in the scene. Once all the trees
#' are labelled the user can exit the tool by selecting an empty region. Points can also be unflagged.
#' The goal of this tool is mainly for minor correction of automatically-detected tree outputs. \cr
#' **This algorithm does not preserve tree IDs from `detected` and renumber all trees. It also looses
#' all attributes**
#'
#' @param detected `SpatialPoints* or `sf/sfc_POINT` with  2 or 3D points of already found tree tops
#' that need manual correction. Can be NULL
#' @param radius numeric. Radius of the spheres displayed on the point cloud (aesthetic purposes only).
#' @param color character. Colour of the spheres displayed on the point cloud (aesthetic purposes only).
#' @param button Which button to use for selection. One of "left", "middle", "right". lidR using left
#' for rotation and right for dragging using one of left or right will disable either rotation or dragging
#' @param ... supplementary parameters to be passed to \link{plot}.
#'
#' @family individual tree detection algorithms
#'
#' @export
#' @md
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # Full manual tree detection
#' ttops = locate_trees(las, manual())
#'
#' # Automatic detection with manual correction
#' ttops = locate_trees(las, lmf(5))
#' ttops = locate_trees(las, manual(ttops))
#' }
#' @name itd_manual
manual = function(detected = NULL, radius = 0.5, color = "red", button = "middle", ...) # nocov start
{
  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTITD, "manual")

    . <- z <- X <- Y <- Z <- treeID <- NULL

    stopifnotlas(las)
    crs <- sf::st_crs(las)

    if (!interactive())
      stop("R is not being used interactively", call. = FALSE)

    if (is.null(detected))
    {
      apice <- data.table::data.table(X = numeric(0), Y = numeric(0), Z = numeric(0))
    }
    else
    {
      if (inherits(detected, "SpatialPoints"))
        detected <- sf::st_as_sf(detected)

      if (!is(detected, "sf") & !is(detected, "sfc"))
        stop("'apice' is not a SpatialPointsDataFrame or sf")

      coords   <- sf::st_coordinates(detected)
      u   <- coords[,1]
      v   <- coords[,2]

      if (ncol(coords) == 3)
        w <- coords[,3]
      else
        w <- detected[["Z"]]

      apice <- data.table::data.table(X = u, Y = v, Z = w)
    }

    minx <- min(las$X)
    miny <- min(las$Y)

    las@data <- las@data[, .(X, Y, Z)]
    las@data[, X := X - minx]
    las@data[, Y := Y - miny]
    apice$X = apice$X - minx
    apice$Y = apice$Y - miny

    plot.LAS(las, ..., clear_artifacts = FALSE)

    id = numeric(nrow(apice))

    # It's very slow to redraw after every sphere, so
    # turn off updates for a bit
    saveSkip = rgl::par3d(skipRedraw = TRUE)
    on.exit(rgl::par3d(saveSkip))  # Just in case of error

    for (i in 1:nrow(apice))
      id[i] = rgl::spheres3d(apice$X[i], apice$Y[i], apice$Z[i], radius = radius, color = color)

    # Now restore drawing mode
    rgl::par3d(saveSkip)
    on.exit() # exit restore no longer needed

    apice$id <- id

    repeat
    {
      # Select a region
      f <- rgl::select3d(button = button)

      # Get the apices in the selected region
      i <- if (nrow(apice) > 0) f(apice) else FALSE

      # There are some apices in the selected region: remove them
      if (sum(i) > 0)
      {
        ii <- which(i == TRUE)
        rgl::pop3d(id = apice[ii]$id)
        apice <- apice[-ii]
      }
      # There is no apex in the selected region: find an apex
      else
      {
        # Get the points in the selected region
        i <- f(las@data)

        # There is 0 points is the region: exit the function
        if (sum(i) == 0)
          break;

        # There are some points: find the highest one and add it to the list of apices
        pts     <- las@data[i, .(X,Y,Z)]
        apex    <- unique(pts[pts$Z == max(pts$Z)])[1]
        apex$id <- as.numeric(rgl::spheres3d(apex$X, apex$Y, apex$Z, radius = radius, color = color))
        apice   <- rbind(apice, apex)
      }
    }

    rgl::close3d()

    apice[, id := NULL]
    apice[, treeID := 1:.N]
    apice[, X := X + minx]
    apice[, Y := Y + miny]
    apice[, z := Z]
    output <- sf::st_as_sf(apice, coords = c("X", "Y", "z"), crs = crs)
    output = output[, c(2,1,3)]
    return(output)
  }

  f <- plugin_itd(f, omp = FALSE, raster_based = FALSE)
  return(f)
} # nocov end
