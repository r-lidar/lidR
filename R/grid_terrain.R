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



#' Digital Terrain Model
#'
#' Interpolates the ground points and creates a rasterized digital terrain model. The algorithm uses
#' the points classified as "ground" and "water" (Classification = 2 and 9, respectively, according to
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{LAS file format specifications})
#' to compute the interpolation.\cr\cr
#' How well the edges of the dataset are interpolated depends on the interpolation method used.
#' A buffer around the region of interest is always recommended to avoid edge effects.
#'
#' @template param-las
#'
#' @template param-res-grid
#'
#' @param algorithm function. A function that implements an algorithm to compute spatial interpolation.
#' \code{lidR} implements \link{knnidw}, \link{tin}, and \link{kriging} (see respective documentation and examples).
#' @param ... Unused
#' @param keep_lowest logical. This option forces the original lowest ground point of each
#' cell (if it exists) to be chosen instead of the interpolated values.
#' @param full_raster logical. By default the interpolation is made only within the convex hull of
#' the point cloud. This prevents meaningless interpolations where there is no data. If TRUE,
#' each pixel of the raster is interpolated.
#' @param use_class integer vector. By default the terrain is computed by using ground points
#' (class 2) and water points (class 9).
#' @param Wdegenerated logical. The function always checks and removes degenerated ground points
#' for computing the DTM to avoid unexpected behaviours, such as infinite elevation. If
#' TRUE, a warning is thrown to alert users to the presence of degenerated ground points.
#' @param is_concave boolean. By default the function tries to compute a DTM that
#' has the same shape as the point cloud by interpolating only in the convex
#' hull of the points. If the point cloud is concave this may lead to weird values
#' where there are no points. Use \code{is_concave = TRUE} to use a concave hull.
#' This is more computationally -involved and requires the concaveman package.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_terrain
#'
#' @template return-grid-Layer
#'
#' @export
#'
#' @examples
#'
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#' #plot(las)
#'
#' dtm1 = grid_terrain(las, algorithm = knnidw(k = 6L, p = 2))
#' dtm2 = grid_terrain(las, algorithm = tin())
#'
#' \dontrun{
#' dtm3 = grid_terrain(las, algorithm = kriging(k = 10L))
#'
#' plot(dtm1)
#' plot(dtm2)
#' plot(dtm3)
#' plot_dtm3d(dtm1)
#' plot_dtm3d(dtm2)
#' plot_dtm3d(dtm3)
#' }
grid_terrain = function(las, res = 1, algorithm, ..., keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE, is_concave = FALSE)
{
  UseMethod("grid_terrain", las)
}

#' @export
grid_terrain.LAS = function(las, res = 1, algorithm, ..., keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE, is_concave = FALSE)
{
  # Defensive programming
  if (!is_a_number(res) & !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer", call. = FALSE)
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_spi(algorithm)
  assert_is_a_bool(keep_lowest)
  assert_is_a_bool(full_raster)
  assert_is_a_bool(is_concave)
  if (full_raster & is_concave) stop("'full_raster' and 'is_concave' cannot both be 'TRUE'", call. = FALSE)
  if (!"Classification" %in% names(las@data)) stop("LAS object does not contain 'Classification' attribute", call. = FALSE)
  if (any(as.integer(use_class) != use_class)) stop("'add_class' is not a vector of integers'", call. = FALSE)
  use_class <- as.integer(use_class)

  # Special non-documented optimization to bypass .availableRAM() from raster
  # In some specific cases where I want to create sequentially many tiny DTMs
  # In this case safety checks from raster become the bottlenecks
  ellipsis <- list(...)
  fast <- isTRUE(ellipsis[["fast"]])

  # Non standard evaluation (R CMD check)
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  # Select the ground points
  ground  <- las@data[Classification %in% c(use_class), .(X,Y,Z)]
  if (nrow(ground) == 0) stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)
  ground  <- check_degenerated_points(ground, Wdegenerated)

  # Find where to interpolate the DTM (interpolation into the convex hull + buffer only).
  verbose("Generating interpolation coordinates...")
  layout <- rOverlay(las, res)
  names(layout) <- "Z"
  grid <- raster2dataframe(layout, xy = TRUE, fast = fast)
  data.table::setDT(grid)
  grid[, Z := NULL]
  data.table::setnames(grid, names(grid), c("X", "Y"))

  # Unless we want to interpolate the whole raster we want to actually generate
  # a DTM that have the same shape than the point cloud
  if (!full_raster)
  {
    if (is_concave)
    {
      assert_package_is_installed("concaveman")
      hull <- concaveman::concaveman(as.matrix(coordinates(las)), 10, 100)
    }
    else
    {
      hull <- convex_hull(las@data$X, las@data$Y)
    }

    hull <- sp::Polygon(hull)
    hull <- sp::SpatialPolygons(list(sp::Polygons(list(hull), "null")))
    hull <- rgeos::gBuffer(hull, width = raster::res(layout)[1])
    hull <- hull@polygons[[1]]@Polygons[[1]]@coords
    keep <- sp::point.in.polygon(grid$X, grid$Y, hull[,1], hull[,2], TRUE) > 0
    if (!all(keep)) grid = grid[keep]
  }

  # Interpolate the terrain providing what to interpolate (ground) and where
  # to interpolate (grid)
  verbose("Interpolating ground points...")
  lidR.context <- "grid_terrain"
  ground <- LAS(ground, las@header, proj4string = las@proj4string, check = FALSE, index = las@index)
  Zg <- algorithm(ground, grid)
  fast_quantization(Zg, las@header@PHB[["Z scale factor"]], las@header@PHB[["Z offset"]])
  cells <- raster::cellFromXY(layout, grid[, .(X,Y)])

  if (fast) {
    layout@data@values[cells] <- Zg
    layout@data@inmemory <- TRUE
  } else {
    suppressWarnings(layout[cells] <- Zg)
  }

  # Replace the interpolated value by the lowest point (legacy code)
  if (keep_lowest)
  {
    verbose("Forcing the lowest ground points to be retained...")
    reso <- raster::res(layout)[1]
    lasg <- filter_poi(las, Classification %in% c(use_class))
    rmin <- grid_metrics(lasg, ~list(Z = min(Z)), reso)
    suppressWarnings(layout[] <- pmin(layout[], rmin[], na.rm = TRUE))
  }

  return(layout)
}

#' @export
grid_terrain.LAScluster = function(las, res = 1, algorithm, ...,  keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE, is_concave = FALSE)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox <- raster::extent(las)
  dtm  <- grid_terrain(x, res, algorithm, keep_lowest = keep_lowest, full_raster = full_raster, use_class = use_class, Wdegenerated = Wdegenerated, is_concave = is_concave)
  dtm  <- raster::crop(dtm, bbox)
  raster::crs(dtm) <- crs(x) # patch for raster not updated with rgal 1.5-8
  return(dtm)
}

#' @export
grid_terrain.LAScatalog = function(las, res = 1, algorithm, ..., keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE, is_concave = FALSE)
{
  # Defensive programming
  if (!is_a_number(res) & !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_spi(algorithm)

  # Enforce some options
  opt_select(las) <- "xyzc"
  #opt_filter(las) <-  paste("-keep_class", paste(use_class, collapse = " "), opt_filter(las))

  # Compute the alignment option including the case when res is a RasterLayer
  alignment   <- list(res = res, start = c(0,0))
  if (is(res, "RasterLayer"))
  {
    ext       <- raster::extent(res)
    r         <- raster::res(res)[1]
    las       <- catalog_intersect(las, res)
    start     <- c(ext@xmin, ext@ymin)
    alignment <- list(res = r, start = start)
  }

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Processing
  options <- list(need_buffer = TRUE, drop_null = TRUE, raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_apply(las, grid_terrain, res = res, algorithm = algorithm, ..., keep_lowest = keep_lowest, full_raster = full_raster, use_class = use_class, Wdegenerated = Wdegenerated, is_concave = is_concave, .options = options)
  return(output)
}
