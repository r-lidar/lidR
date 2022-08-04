# ====== POINTS-TO-RASTER =======

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{rasterize_canopy}. It implements an algorithm for digital
#' surface model computation based on a points-to-raster method: for each pixel of the output raster
#' the function attributes the height of the highest point found. The \code{subcircle} tweak replaces
#' each point with 8 points around the original one. This allows for virtual 'emulation' of the fact
#' that a lidar point is not a point as such, but more realistically a disc. This tweak densifies the
#' point cloud and the resulting canopy model is smoother and contains fewer 'pits' and empty pixels.
#'
#' @param subcircle numeric. Radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#'
#' @param na.fill function. A function that implements an algorithm to compute spatial interpolation
#' to fill the empty pixel often left by points-to-raster methods. \code{lidR} has \link{knnidw},
#' \link{tin}, and \link{kriging} (see also \link{rasterize_terrain} for more details).
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Points-to-raster algorithm with a resolution of 1 meter
#' chm <- rasterize_canopy(las, res = 1, p2r())
#' plot(chm, col = col)
#'
#' # Points-to-raster algorithm with a resolution of 0.5 meters replacing each
#' # point by a 20 cm radius circle of 8 points
#' chm <- rasterize_canopy(las, res = 0.5, p2r(0.2))
#' plot(chm, col = col)
#'
#' \dontrun{
#' chm <- rasterize_canopy(las, res = 0.5, p2r(0.2, na.fill = tin()))
#' plot(chm, col = col)
#' }
#' @name dsm_point2raster
p2r = function(subcircle = 0, na.fill = NULL)
{
  assert_is_a_number(subcircle)
  assert_all_are_non_negative(subcircle)

  subcircle <- lazyeval::uq(subcircle)
  na.fill   <- lazyeval::uq(na.fill)

  if (!is.null(na.fill) && !is(na.fill, "SpatialInterpolation"))
    stop("'na.fill' is not an algorithm for spatial interpolation")

  f = function(las, layout)
  {
    assert_is_valid_context(LIDRCONTEXTDSM, "p2r")

    dsm <- fasterize(las, layout, subcircle, "max")

    if (!is.null(na.fill))
    {
      verbose("Interpolating empty cells...")

      layout[[1]][] <- NA_real_
      layout[[1]][] <- dsm
      hull <- st_convex_hull(las)
      hull <- sf::st_buffer(hull, dist = raster_res(layout)[1])

      where <- raster_as_dataframe(layout, xy = FALSE, na.rm = FALSE)
      where <- where[is.na(where$Z)]
      where <- sf::st_multipoint(as.matrix(where[,1:2]))
      where <- sf::st_geometry(where)
      where <- sf::st_set_crs(where, sf::st_crs(hull))
      where <- sf::st_intersection(where, hull)
      where <- sf::st_coordinates(where)[,-3]
      where <- as.data.frame(where)
      data.table::setnames(where, c("X", "Y"))

      grid  <- raster_as_las(layout)

      lidR.context <- "p2r"
      cells <- raster_cell_from_xy(layout, where$X, where$Y)
      layout[[1]][cells] <- na.fill(grid, where)

      dsm <- as.numeric(layout[[1]])
    }

    return(dsm)
  }

  f <- plugin_dsm(f)
  return(f)
}

# ====== STRICT TRIANGULATION =======

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{rasterize_canopy}. It implements an algorithm for digital
#' surface model computation using a Delaunay triangulation of first returns with a linear interpolation
#' within each triangle.
#'
#' @param max_edge numeric. Maximum edge length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge length greater than this value it will be removed to trim dummy interpolation
#' on non-convex areas. If \code{max_edge = 0} no trimming is done (see examples).
#' @param highest bool. By default it keeps only the highest point per pixel before to triangulate to
#' decrease computation time. If highest = FALSE all first returns are used.
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile)
#' col <- height.colors(50)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- rasterize_canopy(las, res = 1, dsmtin())
#' plot(chm, col = col)
#'
#' \dontrun{
#' # Potentially complex concave subset of point cloud
#' x = c(481340, 481340, 481280, 481300, 481280, 481340)
#' y = c(3812940, 3813000, 3813000, 3812960, 3812940, 3812940)
#' las2 = clip_polygon(las,x,y)
#' plot(las2)
#'
#' # Because the TIN interpolation is done within the convex hull of the point cloud
#' # dummy pixels are interpolated that are correct according to the interpolation method
#' # used, but meaningless in our CHM
#' chm <- rasterize_canopy(las2, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Use 'max_edge' to trim dummy triangles
#' chm = rasterize_canopy(las2, res = 0.5, dsmtin(max_edge = 3))
#' plot(chm, col = col)
#' }
#' @name dsm_tin
dsmtin = function(max_edge = 0, highest = TRUE)
{
  max_edge <- lazyeval::uq(max_edge)
  return(pitfree(0, c(max_edge, 0), 0, highest))
}

# ====== PIT-FREE =======

#' Digital Surface Model Algorithm
#'
#' This function is made to be used in \link{rasterize_canopy}. It implements the pit-free algorithm
#' developed by Khosravipour et al. (2014), which is based on the computation of a set of classical
#' triangulations at different heights (see references). The \code{subcircle} tweak replaces each
#' point with 8 points around the original one. This allows for virtual 'emulation' of the fact that
#' a lidar point is not a point as such, but more realistically a disc. This tweak densifies the point
#' cloud and the resulting canopy model is smoother and contains fewer 'pits' and empty pixels.
#'
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#' @param thresholds numeric. Set of height thresholds according to the Khosravipour et al. (2014) algorithm
#' description (see references)
#' @param max_edge numeric. Maximum edge length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge length greater than this value it will be removed. The first number is the value
#' for the classical triangulation (threshold = 0, see also \link{dsmtin}), the second number
#' is the value for the pit-free algorithm (for thresholds > 0). If \code{max_edge = 0} no trimming
#' is done (see examples).
#' @param highest bool. By default it keeps only the highest point per pixel before to triangulate to
#' decrease computation time. If highest = FALSE all first returns are used.
#'
#' @references Khosravipour, A., Skidmore, A. K., Isenburg, M., Wang, T., & Hussin, Y. A. (2014).
#' Generating pit-free canopy height models from airborne lidar. Photogrammetric Engineering &
#' Remote Sensing, 80(9), 863-872.
#'
#' @export
#'
#' @family digital surface model algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' poi = "-drop_z_below 0 -inside 481280 3812940 481330 3812990"
#' las <- readLAS(LASfile, filter = poi)
#' col <- height.colors(50)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- rasterize_canopy(las, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm <- rasterize_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
#' plot(chm, col = col)
#'
#' \dontrun{
#' # Potentially complex concave subset of point cloud
#' x = c(481340, 481340, 481280, 481300, 481280, 481340)
#' y = c(3812940, 3813000, 3813000, 3812960, 3812940, 3812940)
#' las2 = clip_polygon(las,x,y)
#' plot(las2)
#'
#' # Because the TIN interpolation is done within the convex hull of the point cloud
#' # dummy pixels are interpolated that are correct according to the interpolation
#' # method used, but meaningless in our CHM
#' chm <- rasterize_canopy(las2, res = 0.5, pitfree(max_edge = c(0, 1.5)))
#' plot(chm, col = col)
#'
#' chm = rasterize_canopy(las2, res = 0.5, pitfree(max_edge = c(3, 1.5)))
#' plot(chm, col = col)
#' }
#' @export
#' @name dsm_pitfree
pitfree <- function(thresholds = c(0, 2, 5, 10, 15), max_edge = c(0, 1), subcircle = 0, highest = TRUE)
{
  assert_is_numeric(thresholds)
  assert_all_are_non_negative(thresholds)
  assert_is_numeric(max_edge)
  assert_all_are_non_negative(max_edge)
  assert_is_a_number(subcircle)
  assert_all_are_non_negative(subcircle)
  assert_is_a_bool(highest)

  if (length(thresholds) > 1L & length(max_edge) < 2L)
  {
    stop("'max_edge' should contain 2 numbers")
  }

  thresholds <- lazyeval::uq(thresholds)
  max_edge <- lazyeval::uq(max_edge)
  subcircle <- lazyeval::uq(subcircle)
  highest <- lazyeval::uq(highest)

  f <- function(las, layout)
  {
    assert_is_valid_context(LIDRCONTEXTDSM, "pitfree")

    if (!"ReturnNumber" %in% names(las)) stop("No attribute 'ReturnNumber' found. This attribute is needed to extract first returns", call. = FALSE)
    if (fast_countequal(las$ReturnNumber, 1L) == 0) stop("No first returns found. Operation aborted.", call. = FALSE)

    # Non standard evaluation (R CMD check)
    . <- .N <- X <- Y <- Z <- ReturnNumber <- NULL

    # Delaunay triangulation with boost requiere to
    # compute back integer coordinates
    xscale  <- las[["X scale factor"]]
    yscale  <- las[["Y scale factor"]]
    xoffset <- las[["X offset"]]
    yoffset <- las[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)

    # Get only first returns and coordinates (nothing else needed)
    verbose("Selecting first returns...")
    cloud <- las@data[ReturnNumber == 1L, .(X, Y, Z)]
    cloud <- LAS(cloud, las@header, st_crs(las), check = FALSE, index = las@index)

    # subcircle the data
    if (subcircle > 0)
    {
      verbose("Subcircling points...")
      cloud <- subcircle(cloud, subcircle, 8L)
    }

    if (highest)
    {
      verbose("Selecting only the highest points within the grid cells...")
      template <- raster_template(layout)
      i <- C_highest(cloud, template)
      cloud <- cloud[i]
      if (nrow(cloud) < 3) stop("There are not enought points to triangulate.", call. = FALSE)
    }

    # Get interpolation grid
    grid <- raster_as_dataframe(layout, na.rm = FALSE)

    # Initialize the interpolated values with NAs
    z <- rep(NA_real_, nrow(grid))

    # Perform the triangulation and the rasterization (1 loop for classical triangulation, several for Khosravipour et al.)
    thresholds <- sort(thresholds)
    for (i in seq_along(thresholds))
    {
      verbose(glue::glue("Triangulation pass {i} of {length(thresholds)}..."))
      th <- thresholds[i]
      edge <- if (th == 0) max_edge[1] else max_edge[2]

      if (fast_countover(cloud$Z, th) > 3)
      {
        b <- cloud$Z >= th
        cloud <- cloud[b]
        Ztemp <- interpolate_delaunay(cloud@data, grid, edge, scales, offsets)

        if (i == 1 && all(is.na(Ztemp)))
        {
          stop("Interpolation failed in the first layer (NAs everywhere). Maybe there are too few points.", call. = FALSE)
        }

        z <- pmax(z, Ztemp, na.rm = T)
      }
    }

    if (all(is.na(z)))
      stop("Interpolation failed (NAs everywhere). Input parameters might be wrong.", call. = FALSE)

    return(z)
  }

  f <- plugin_dsm(f, omp = TRUE)
  return(f)
}

subcircle = function(las, r, n)
{
  xscale  <- las[["X scale factor"]]
  yscale  <- las[["Y scale factor"]]
  xoffset <- las[["X offset"]]
  yoffset <- las[["Y offset"]]
  bbox    <- st_bbox(las)

  dt <- las@data
  X <- Y <- Z <- NULL

  f = function(x, y, z, px, py)
  {
    x = x + px
    y = y + py
    z = rep(z, length(px))

    list(X = x, Y = y, Z = z)
  }

  n = n + 1

  alpha = seq(0, 2*pi, length.out = n)[-n]
  px = r*cos(alpha)
  py = r*sin(alpha)

  dt <- dt[, f(X, Y, Z, px, py), by = 1:nrow(dt)][, nrow := NULL][]
  dt <- dt[data.table::between(X, bbox$xmin, bbox$xmax) & data.table::between(Y, bbox$ymin, bbox$ymax)]
  dt[1:.N, `:=`(X = round_any(X - xoffset, xscale) + xoffset, Y = round_any(Y - yoffset, yscale) + yoffset)]
  return(LAS(dt, las@header, st_crs(las), check = FALSE, index = las@index))
}
