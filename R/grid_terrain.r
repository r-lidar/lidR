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
#' Depending on the interpolation method, the edges of the dataset can be more, or less poorly
#' interpolated. A buffer around the region of interest is always recommended to avoid edge
#' effects.
#'
#' @template param-las
#'
#' @template param-res-grid
#'
#' @param algorithm function. A function that implements an algorithm to compute spatial interpolation.
#' \code{lidR} have \link{knnidw}, \link{tin}, \link{kriging} (see respective documentations and exemples).
#'
#' @param keep_lowest logical. This option forces the original lowest ground point of each
#' cell (if it exists) to be chosen instead of the interpolated values.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-Layer
#'
#' @seealso
#' \link{lasnormalize}
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
#' plot3d(dtm1)
#' plot3d(dtm2)
#' plot3d(dtm3)
#' }
grid_terrain = function(las, res = 1, algorithm, keep_lowest = FALSE)
{
  if(!assertive::is_a_number(res) & !is(res, "RasterLayer"))
    stop("res is not a number or a RasterLayer", call. = FALSE)

  if(assertive::is_a_number(res))
    assertive::assert_all_are_non_negative(res)

  UseMethod("grid_terrain", las)
}

#' @export
grid_terrain.LAS = function(las, res = 1, algorithm, keep_lowest = FALSE)
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "SpatialInterpolation"))
    stop("The algorithm is not an algorithm for spatial interpolation", call. = FALSE)

  . <- X <- Y <- Z <- Classification <- NULL

  # Select the ground points
  # ========================

  if (!"Classification" %in% names(las@data))
    stop("LAS object does not contain 'Classification' data", call. = FALSE)

  if (fast_countequal(las@data$Classification, 2L) == 0)
    stop("No ground points found. Impossible to compute a DTM.", call. = FALSE)

  ground = las@data[Classification == LASGROUND, .(X,Y,Z)]

  # Find where to interpolate the DTM
  # =================================

  verbose("Generating interpolation coordinates...")

  layout <- make_overlay_raster(las, res)

  names(layout) <- "Z"
  grid <- raster::as.data.frame(layout, xy = TRUE)
  data.table::setDT(grid)
  grid[, Z := NULL]
  data.table::setnames(grid, names(grid), c("X", "Y"))

  hull <- convex_hull(las@data$X, las@data$Y)
  hull <- sp::Polygon(hull)
  hull <- sp::SpatialPolygons(list(sp::Polygons(list(hull), "null")))
  hull <- rgeos::gBuffer(hull, width = raster::res(layout)[1])
  hull <- hull@polygons[[1]]@Polygons[[1]]@coords
  keep <- C_points_in_polygon(hull[,1], hull[,2], grid$X, grid$Y)
  if (!all(keep)) grid = grid[keep]


  # Interpolate the terrain
  # =======================

  has_buffer <- "buffer" %in% names(las@data)

  verbose("Interpolating ground points...")

  lidR.context <- "grid_terrain"
  Zg <- algorithm(ground, grid)

  cells <- raster::cellFromXY(layout, grid[, .(X,Y)])
  suppressWarnings(layout[cells] <- Zg)

  if (keep_lowest)
  {
    verbose("Forcing the lowest ground points to be retained...")
    rmin <- grid_metrics(lasfilterground(las), list(Z = min(Z)), raster::res(layout)[1])
    layout[] <- pmin(layout[], rmin[])
  }

  return(layout)
}

#' @export
grid_terrain.LAScluster= function(las, res = 1, algorithm, keep_lowest = FALSE)
{
  x = readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox <- raster::extent(las)
  dtm  <- grid_terrain(x, res, algorithm, keep_lowest)
  dtm  <- raster::crop(dtm, bbox)
  return(dtm)
}

#' @export
grid_terrain.LAScatalog = function(las, res = 1, algorithm, keep_lowest = FALSE)
{
  if (is(res, "RasterLayer"))
  {
    ext = raster::extent(res)
    keep = with(las@data, !(`Min X` >= ext@xmax | `Max X` <= ext@xmin | `Min Y` >= ext@ymax | `Max Y` <= ext@ymin))
    las = las[keep,]
  }

  set_select(las) <- "xyzc"
  output <- catalog_apply2(las, grid_terrain, res = res, algorithm = algorithm, keep_lowest = keep_lowest, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE)

  if (get_output_files(las) != "")                  # Outputs have been written in files. Return a virtual raster mosaic
    return(build_vrt(output, "grid_terrain"))
  else                                              # Outputs have been returned in R objects. Merge the outputs in a single object
    return(merge_rasters(output))
}