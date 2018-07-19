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
#' @section Use with a \code{LAScatalog}:
#' When the parameter \code{x} is a \link[lidR:LAScatalog-class]{LAScatalog} the function processes
#' the entire dataset in a continuous way using a multicore process. The user can modify the processing
#' options using the \link[lidR:catalog]{available options}.\cr\cr
#' \code{lidR} supports .lax files. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a LAScatalog")
#' @param res numeric. resolution.
#' @param method character. can be \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see details)
#' @param k numeric. number of k-nearest neighbours when the selected method is either \code{"knnidw"} or \code{"kriging"}
#' @param p numeric. Power for inverse-distance weighting. Default 2.
#' @param model a variogram model computed with \link[gstat:vgm]{vgm} when the selected method
#' is \code{"kriging"}. If null, it performs an ordinary or weighted least squares prediction.
#' @param keep_lowest logical. This function forces the original lowest ground point of each
#' pixel (if it exists) to be chosen instead of the interpolated values.
#' @return A \code{lasmetrics} data.table.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' lidar = readLAS(LASfile)
#' plot(lidar)
#'
#' dtm1 = grid_terrain(lidar, method = "knnidw", k = 6, p = 2)
#' dtm2 = grid_terrain(lidar, method = "delaunay")
#' dtm3 = grid_terrain(lidar, method = "kriging", k = 10)
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
#' \link[lidR:grid_terrain]{grid_terrain}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[gstat:vgm]{vgm}
#' \link[gstat:krige]{krige}
#' \link[lidR:lasnormalize]{lasnormalize}
#' \link[raster:raster]{RasterLayer}
grid_terrain = function(x, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  UseMethod("grid_terrain", x)
}

#' @export
grid_terrain.LAS = function(x, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  . <- X <- Y <- Z <- Classification <- NULL
  resolution = res

  # ========================
  # Select the ground points
  # ========================

  verbose("Selecting ground points...")

  if (!"Classification" %in% names(x@data))
    stop("LAS object does not contain 'Classification' data")

  if (fast_countequal(x@data$Classification, 2L) == 0)
    stop("No ground points found. Impossible to compute a DTM.", call. = F)

  ground = x@data[Classification == LASGROUND, .(X,Y,Z)]

  # =================================
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
    ext  = extent(x) + 0.1 * resolution
    grid = make_grid(ext@xmin, ext@xmax, ext@ymin, ext@ymax, resolution)

    hull = convex_hull(x@data$X, x@data$Y)
    hull = sp::Polygon(hull)
    hull = sp::SpatialPolygons(list(sp::Polygons(list(hull), "null")))
    hull = rgeos::gBuffer(hull, width = resolution)
    hull = hull@polygons[[1]]@Polygons[[1]]@coords
    keep = C_points_in_polygon(hull[,1], hull[,2], grid$X, grid$Y)
    grid = grid[keep]
  }

  # =======================
  # Interpolate the terrain
  # =======================

  has_buffer = "buffer" %in% names(x@data)

  verbose("Interpolating ground points...")

  Zg = interpolate(ground, grid, method, k, p, model, wbuffer = !has_buffer)
  grid[, Z := round(Zg, 3)][]

  if (keep_lowest)
  {
    verbose("Forcing the lowest ground points to be retained...")

    grid = rbind(grid, grid_metrics(lasfilterground(x), list(Z = min(Z)), resolution))
    grid = grid[, list(Z = min(Z)), by = .(X,Y)]
  }

  as.lasmetrics(grid, resolution)

  return(grid)
}

#' @export
grid_terrain.LAScatalog = function(x, res = 1, method, k = 10L, p = 2, model = gstat::vgm(.59, "Sph", 874), keep_lowest = FALSE)
{
  terrain = grid_catalog(x, grid_terrain, res, "xyzc", "", method = method, k = k, p = p, model = model, keep_lowest = keep_lowest)

  return(terrain)
}