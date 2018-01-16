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



#' Canopy surface model
#'
#' Creates a canopy surface model using a LiDAR point cloud. For each pixel the function
#' returns the highest point found (point-to-raster). This basic method could be improved
#' by replacing each LiDAR return with a small disk. An interpolation for empty pixels is
#' also available.
#'
#' The algorithm relies on a point-to-raster approach. For each pixel the elevation of the
#' highest point is found and attributed to this pixel. This method implies that the resulting
#' surface model can contain empty pixels. Those 'holes' can be filled by interpolation.
#' Internally, the interpolation is based on the same method used in the function
#' \link[lidR:grid_terrain]{grid_terrain}. Therefore the documentation for
#' \link[lidR:grid_terrain]{grid_terrain} is also applicable to this function (see also
#' examples).\cr\cr
#' The 'subcircle' tweak replaces each point with 8 points around the original one. This allows
#' for virtual 'emulation' of the fact that a lidar point is not a point as such, but more
#' realistically a disc. This tweak densifies the point cloud and the resulting canopy model is
#' smoother and contains fewer 'pits' and empty pixels.
#'
#' @section Use with a \code{LAScatalog}:
#' When the parameter \code{x} is a \link[lidR:LAScatalog-class]{LAScatalog} the function processes
#' the entire dataset in a continuous way using a multicore process. Parallel computing is set
#' by default to the number of core available in the computer. The user can modify the global
#' options using the function \link{catalog_options}.\cr\cr
#' \code{lidR} support .lax file. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @aliases  grid_canopy
#' @param x An object of class \link{LAS} or a \link{catalog} (see section "Use with a LAScatalog")
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is
#' 2 meters i.e. 4 square meters.
#' @param subcircle numeric. radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points (see details).
#' @param na.fill character. name of the algorithm used to interpolate the data and fill the empty pixels.
#' Can be \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see details).
#' @param ... extra parameters for the algorithm used to interpolate the empty pixels (see details)
#' @param filter character. Streaming filter while reading the files (see \link{readLAS}).
#' If \code{x} is a \code{LAScatalog} the function \link{readLAS} is called internally. The
#' user cannot manipulate the lidar data directly but can use streaming filters instead.
#' @return Returns a \code{data.table} of class \code{lasmetrics}, which enables easier
#' plotting and RasterLayer casting.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Local maximum algorithm with a resolution of 2 meters
#' lidar %>% grid_canopy(2) %>% plot
#'
#' # Local maximum algorithm with a resolution of 1 meter replacing each
#' # point by a 20 cm radius circle of 8 points
#' lidar %>% grid_canopy(1, 0.2) %>% plot
#'
#' # Local maximum algorithm with a resolution of 1 meter replacing each
#' # point by a 10 cm radius circle of 8 points and interpolating the empty
#' # pixels using the 3-nearest neighbours and an inverse-distance weighting.
#' grid_canopy (lidar, 1, subcircle = 0.1, na.fill = "knnidw", k = 3, p = 2) %>% plot
#'
#' \dontrun{
#' grid_canopy(lidar, 1, na.fill = "knnidw", k = 3) %>% plot
#' grid_canopy(lidar, 1, subcircle = 0.1, na.fill = "delaunay") %>% plot
#' }
#' @family grid_alias
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:as.raster.lasmetrics]{as.raster}
#' @export grid_canopy
grid_canopy = function(x, res = 2, subcircle = 0, na.fill = "none", ..., filter = "")
{
  UseMethod("grid_canopy", x)
}

#' @export
grid_canopy.LAS = function(x, res = 2, subcircle = 0, na.fill = "none", ..., filter = "")
{
  . <- X <- Y <- Z <- NULL

  if (!is.numeric(res))
    stop("Argument 'res' should be a number", call. = FALSE)

  if (res < 0)
    stop("Argument 'res' should be greater than 0", call. = FALSE)

  if (!is.numeric(subcircle))
    stop("Argument 'subcircle' should be a number", call. = FALSE)

  if (subcircle < 0)
    stop("Argument 'subcircle' should be greater than 0", call. = FALSE)

  verbose("Gridding highest points in each cell...")

  dsm = Cpp_grid_canopy(x, res, subcircle)
  as.lasmetrics(dsm, res)

  if (na.fill != "none")
  {
    verbose("Interpolating empty cells...")

    ex = extent(x)
    grid = make_grid(ex@xmin, ex@xmax, ex@ymin, ex@ymax, res)

    hull = convex_hull(x@data$X, x@data$Y)

    # buffer around convex hull
    sphull = sp::Polygon(hull)
    sphull = sp::SpatialPolygons(list(sp::Polygons(list(sphull), "null")))
    hull = rgeos::gBuffer(sphull, width = res)
    hull = hull@polygons[[1]]@Polygons[[1]]@coords

    grid = grid[points_in_polygon(hull[,1], hull[,2], grid$X, grid$Y)]

    data.table::setkeyv(grid, c("X", "Y"))
    data.table::setkeyv(dsm, c("X", "Y"))
    data.table::setattr(dsm, "class", class(grid))

    dsm = dsm[grid]

    z = interpolate(dsm[!is.na(Z)], dsm[is.na(Z)], method = na.fill, ...)

    dsm[is.na(Z), Z := z]

    as.lasmetrics(dsm, res)
  }

  return(dsm)
}

#' @export
grid_canopy.LAScatalog = function(x, res = 2, subcircle = 0, na.fill = "none", ..., filter = "")
{
  oldbuffer <- CATALOGOPTIONS("buffer")

  CATALOGOPTIONS(buffer = res/2 + subcircle)

  canopy = grid_catalog(x, grid_canopy, res, "xyz", filter, subcircle = subcircle, na.fill = na.fill, ...)

  CATALOGOPTIONS(buffer = oldbuffer)

  return(canopy)
}

