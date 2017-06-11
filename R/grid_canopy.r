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
#' Creates a canopy surface model using a LiDAR point cloud. For each pixel the
#' function returns the highest point found. This basic method could be improved by replacing
#' each LiDAR return with a small disk. An interpolation for empty pixels is also available.
#'
#' The algorithm relies on the 'local maximum'. For each pixel the function returns the highest
#' point found. This method implies that the resulting surface model can contain empty pixels.
#' Those 'holes' can be filled by interpolation. Internally, the interpolation is based on the same method
#' used in the function \link[lidR:grid_terrain]{grid_terrain}. Therefore the
#' documentation for \link[lidR:grid_terrain]{grid_terrain} is also applicable to this function.
#' (see also examples)
#' @aliases  grid_canopy
#' @param .las An object of class \code{LAS}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is
#' 2 meters i.e. 4 square meters.
#' @param subcircle numeric radius of the circles. To obtain fewer empty pixels the algorithm
#' can replace each return with a circle composed of 8 points before computing the maximum elevation
#' in each pixel.
#' @param na.fill character. name of the algorithm used to interpolate the data and fill the empty pixels.
#' Can be \code{"knnidw"}, \code{"delaunay"} or \code{"kriging"} (see details).
#' @param ... extra parameters for the algorithm used to interpolate the empty pixels (see details)
#' @return It returns a \code{data.table} with the class \code{lasmetrics}, which enables easier plotting and
#' RasterLayer casting.
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
#' grid_canopy (lidar, 1, subcircle = 0.1, na.fill = "knnidw", k = 3) %>% plot
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
grid_canopy = function(.las, res = 2, subcircle = 0, na.fill = "none", ...)
{
  . <- X <- Y <- Z <- NULL

  if (subcircle > 0)
  {
    verbose("Subcircling points...")

    ex = extent(.las)

    dt = .las@data[, .(X,Y,Z)]
    dt = subcircled(dt, subcircle, 8)
    dt = dt[between(X, ex@xmin, ex@xmax) & between(Y, ex@ymin, ex@ymax)]
    .las = suppressWarnings(LAS(dt))

    rm(dt)
  }

  verbose("Gridding highest points in each cell...")

  dsm   = grid_metrics(.las, list(Z = max(Z)), res)

  if (na.fill != "none")
  {
    verbose("Interpolating empty cells...")

    ex = extent(.las)
    grid = make_grid(ex@xmin, ex@xmax, ex@ymin, ex@ymax, res)

    data.table::setkeyv(grid, c("X", "Y"))
    data.table::setkeyv(dsm, c("X", "Y"))
    data.table::setattr(dsm, "class", class(grid))

    dsm = dsm[grid]

    z = interpolate(dsm[!is.na(Z)], dsm[is.na(Z)], method = na.fill, ...)

    dsm[is.na(Z), Z := z]

    as.lasmetrics(dsm, res)
  }

  rm(.las)
  gc()

  return(dsm)
}
