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
#' the points classified as "ground" (Classification = 2 according to
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format specifications})
#' to compute the interpolation.\cr
#' How well the edges of the dataset are interpolated depends on the interpolation method used.
#' Thus, a buffer around the region of interest is always recommended to avoid edge effects.
#'
#' @template param-las
#'
#' @template param-res-grid
#'
#' @param algorithm function. A function that implements an algorithm to compute spatial interpolation.
#' \code{lidR} implements \link{knnidw}, \link{tin}, and \link{kriging} (see respective documentation and examples).
#'
#' @param keep_lowest logical. This option forces the original lowest ground point of each
#' cell (if it exists) to be chosen instead of the interpolated values.
#' @param full_raster logical. By default the interpolation is made only within the convex hull of
#' the point cloud. This prevent against meaningless interpolations where there is no data. If TRUE
#' each pixel of the raster is interpolated.
#' @param use_class integer vector. By default the terrain is computed by using ground points
#' (class 2) and water points (class 9).
#' @param Wdegenerated logical. The function always check and remove degenerated ground points. If
#' any a warning in thrown.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-Layer
#'
#' @export
#'
#' @examples
#'
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#' plot(las)
#'
#' dtm1 = grid_terrain(las, algorithm = knnidw(k = 6L, p = 2))
#' dtm2 = grid_terrain(las, algorithm = tin())
#' dtm3 = grid_terrain(las, algorithm = kriging(k = 10L))
#'
#' \dontrun{
#' plot(dtm1)
#' plot(dtm2)
#' plot(dtm3)
#' plot_dtm3d(dtm1)
#' plot_dtm3d(dtm2)
#' plot_dtm3d(dtm3)
#' }
grid_terrain = function(las, res = 1, algorithm, keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE)
{
  UseMethod("grid_terrain", las)
}

#' @export
grid_terrain.LAS = function(las, res = 1, algorithm, keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE)
{
  # Defensive programming
  if (!is_a_number(res) & !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer", call. = FALSE)
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_algorithm(algorithm)
  assert_is_algorithm_spi(algorithm)
  assert_is_a_bool(keep_lowest)
  assert_is_a_bool(full_raster)
  if (!"Classification" %in% names(las@data)) stop("LAS object does not contain 'Classification' attribute", call. = FALSE)
  if (any(as.integer(use_class) != use_class)) stop("'add_class' is not a vector of integers'", call. = FALSE)
  use_class <- as.integer(use_class)

  # Non standart evaluation (R CMD check)
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  # Delaunay triangulation with boost requires to
  # compute back integer coordinates
  xscale  <- las@header@PHB[["X scale factor"]]
  yscale  <- las@header@PHB[["Y scale factor"]]
  zscale  <- las@header@PHB[["Z scale factor"]]
  xoffset <- las@header@PHB[["X offset"]]
  yoffset <- las@header@PHB[["Y offset"]]
  scales  <- c(xscale, yscale)
  offsets <- c(xoffset, yoffset)

  # Select the ground points
  ground  <- las@data[Classification %in% c(use_class), .(X,Y,Z)]
  if (nrow(ground) == 0) stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)
  ground  <- check_degenerated_points(ground, Wdegenerated)

  # Find where to interpolate the DTM (interpolation into the convex hull + buffer only).
  verbose("Generating interpolation coordinates...")
  layout <- rOverlay(las, res)
  names(layout) <- "Z"
  grid <- raster::as.data.frame(layout, xy = TRUE)
  data.table::setDT(grid)
  grid[, Z := NULL]
  data.table::setnames(grid, names(grid), c("X", "Y"))

  if (!full_raster) {
    hull <- convex_hull(las@data$X, las@data$Y)
    hull <- sp::Polygon(hull)
    hull <- sp::SpatialPolygons(list(sp::Polygons(list(hull), "null")))
    hull <- rgeos::gBuffer(hull, width = raster::res(layout)[1])
    hull <- hull@polygons[[1]]@Polygons[[1]]@coords
    keep <- sp::point.in.polygon(grid$X, grid$Y, hull[,1], hull[,2], TRUE) > 0
    if (!all(keep)) grid = grid[keep]
  }

  # Interpolate the terrain
  verbose("Interpolating ground points...")
  lidR.context <- "grid_terrain"
  Zg <- algorithm(ground, grid, scales, offsets)
  Zg <- round_any(Zg, zscale)
  cells <- raster::cellFromXY(layout, grid[, .(X,Y)])
  suppressWarnings(layout[cells] <- Zg)

  # Replace the interpolated value by the lowest point
  if (keep_lowest) {
    verbose("Forcing the lowest ground points to be retained...")
    reso <- raster::res(layout)[1]
    lasg <- filter_poi(las, Classification %in% c(use_class))
    rmin <- grid_metrics(lasg, ~list(Z = min(Z)), reso)
    suppressWarnings(layout[] <- pmin(layout[], rmin[], na.rm = TRUE))
  }

  return(layout)
}

#' @export
grid_terrain.LAScluster = function(las, res = 1, algorithm, keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE)
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox <- raster::extent(las)
  dtm  <- grid_terrain(x, res, algorithm, keep_lowest, full_raster, use_class, Wdegenerated)
  dtm  <- raster::crop(dtm, bbox)
  raster::crs(dtm) <- crs(x) # patch for raster not updated with rgal 1.5-8
  return(dtm)
}

#' @export
grid_terrain.LAScatalog = function(las, res = 1, algorithm, keep_lowest = FALSE, full_raster = FALSE, use_class = c(2L,9L), Wdegenerated = TRUE)
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
  output  <- catalog_apply(las, grid_terrain, res = res, algorithm = algorithm, keep_lowest = keep_lowest, full_raster = full_raster, use_class = use_class, Wdegenerated = Wdegenerated, .options = options)
  return(output)
}
