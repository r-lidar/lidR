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
#' Interpolates ground points and creates a rasterized digital terrain model. The interpolation
#' can be done using 3 methods: \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see
#' details). The algorithm uses the points classified as "ground" to compute the interpolation.\cr
#' Depending on the interpolation method, the edges of the dataset can be more, or less poorly
#' interpolated. A buffer around the region of interest is always recommended to avoid edge
#' effects.
#'
#' \describe{
#' \item{\code{knnidw}}{Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse-distance weighting (IDW). This is a fast but basic method for spatial
#' data interpolation.}
#' \item{\code{delaunay}}{Interpolation based on Delaunay triangulation. It makes a linear
#' interpolation within each triangle. There are usually few cells outside the convex hull,
#' determined by the ground points at the very edge of the dataset that cannot be interpolated
#' with a triangulation. Extrapolation is done using knnidw.}
#' \item{\code{kriging}}{Interpolation is done by universal kriging using the
#' \link[gstat:krige]{krige} function. This method combines the KNN approach with the kriging
#' approach. For each point of interest the terrain is kriged using the k-nearest neighbour ground
#' points. This method is more difficult to manipulate but it is also the most advanced method for
#' interpolating spatial data. }
#' }
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template param-las
#' @param res numeric. resolution of the \code{RasterLayer}. Default 1.
#' @param method character. can be \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see details)
#' @param k numeric. number of k-nearest neighbours. Default 10.
#' @param p numeric. Power for inverse-distance weighting. Default 2.
#' @param model a variogram model computed with \link[gstat:vgm]{vgm}. If null, it performs an ordinary
#' or weighted least squares prediction.
#' @param keep_lowest logical. This option forces the original lowest ground point of each
#' cell (if it exists) to be chosen instead of the interpolated values.
#' @param ... parameters for the algorithms. These depend on the algorithm used (see documentation
#' of each method).
#'
#' @template return-grid-Layer
#'
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#' plot(las)
#'
#' dtm1 = grid_terrain(las, method = "knnidw", k = 6L, p = 2)
#' dtm2 = grid_terrain(las, method = "delaunay")
#' dtm3 = grid_terrain(las, method = "kriging", k = 10L)
#'
#' \dontrun{
#' plot(dtm1)
#' plot(dtm2)
#' plot(dtm3)
#' plot3d(dtm1)
#' plot3d(dtm2)
#' plot3d(dtm3)
#' }
#' @seealso
#' \link{lasnormalize}
grid_terrain = function(las, method, ...)
{
  if (method == "delaunay")
    return(grid_terrain_delaunay(las, ...))
  else if (method == "knnidw")
    return(grid_terrain_knnidw(las, ...))
  else if (method == "kriging")
    return(grid_terrain_kriging(las, ...))
  else
    stop(glue::glue("Method '{method}' not supported"), call. = FALSE)
}

#' @export
#' @rdname grid_terrain
grid_terrain_delaunay = function(las, res = 1,  keep_lowest = FALSE)
{
  grid_terrain_generic(las, res = 1, method = "delaunay", keep_lowest = keep_lowest)
}

#' @export
#' @rdname grid_terrain
grid_terrain_knnidw = function(las, res = 1, k = 10L, p = 2, keep_lowest = FALSE)
{
  grid_terrain_generic(las, method = "knnidw", res = res,  k = k, p = p, keep_lowest = keep_lowest)
}

#' @export
#' @rdname grid_terrain
grid_terrain_kriging = function(las, res = 1, k = 10L,  model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  grid_terrain_generic(las, method = "kriging", res = res, k = k, model = model, keep_lowest = keep_lowest)
}


grid_terrain_generic = function(las, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  assertive::assert_is_a_number(k)
  assertive::assert_is_a_number(p)
  assertive::assert_all_are_whole_numbers(k)
  assertive::assert_all_are_positive(k)
  assertive::assert_all_are_positive(p)
  assertive::assert_is_a_bool(keep_lowest)

  UseMethod("grid_terrain_generic", las)
}

grid_terrain_generic.LAS = function(las, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  . <- X <- Y <- Z <- Classification <- NULL
  resolution = res

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

  if (is(res, "RasterLayer"))
  {
    resolution = raster::res(res)

    if (resolution[1] !=  resolution[2])
      stop("Rasters with different x y resolutions are not supported", call. = FALSE)

    resolution = resolution[1]

    grid = raster::xyFromCell(res, 1:raster::ncell(res))
    grid = data.table::as.data.table(grid)
    data.table::setnames(grid, names(grid), c("X", "Y"))
  }
  else
  {
    layout = make_overlay_raster(las, resolution)
    layout@crs = las@proj4string
    names(layout) = "Z"
    grid = raster::as.data.frame(layout, xy = TRUE)
    data.table::setDT(grid)
    grid[, Z := NULL]
    data.table::setnames(grid, names(grid), c("X", "Y"))

    hull = convex_hull(las@data$X, las@data$Y)
    hull = sp::Polygon(hull)
    hull = sp::SpatialPolygons(list(sp::Polygons(list(hull), "null")))
    hull = rgeos::gBuffer(hull, width = resolution)
    hull = hull@polygons[[1]]@Polygons[[1]]@coords
    keep = C_points_in_polygon(hull[,1], hull[,2], grid$X, grid$Y)
    if (!all(keep)) grid = grid[keep]
  }

  # Interpolate the terrain
  # =======================

  has_buffer = "buffer" %in% names(las@data)

  verbose("Interpolating ground points...")

  Zg = interpolate(ground, grid, method, k, p, model, wbuffer = !has_buffer)
  grid[, Z := round(Zg, 3)][]

  cells = raster::cellFromXY(layout, grid[, .(X,Y)])
  suppressWarnings(layout[cells] <- Zg)

  if (keep_lowest)
  {
    verbose("Forcing the lowest ground points to be retained...")
    rmin = grid_metrics(lasfilterground(las), list(Z = min(Z)), resolution)
    layout[] = pmin(layout[], rmin[])
  }

  return(layout)
}

grid_terrain_generic.LAScluster= function(las, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  x = readLAS(las)
  if (is.null(x)) return(NULL)
  bbox <- raster::extent(as.numeric(las@bbox))
  dtm  <- grid_terrain_generic(x, res, method, k, p, model, keep_lowest)
  dtm  <- raster::crop(dtm, bbox)
  return(dtm)
}

grid_terrain_generic.LAScatalog = function(las, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE, ...)
{
  las@input_options$select <- "xyzc"

  output        <- catalog_apply2(las, grid_terrain_generic, res = res, method = method, k = k, p = p, model = model, keep_lowest = keep_lowest, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (output_files(las) != "")  return(unlist(output))

  # Outputs have been return in R objects. Merge the outptus in a single object
  names         <- names(output[[1]])
  factor        <- output[[1]]@data@isfactor
  output        <- do.call(raster::merge, output)
  output@crs    <- las@proj4string
  names(output) <- names
  return(output)
}