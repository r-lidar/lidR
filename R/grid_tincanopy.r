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

#' Canopy height model based on a triangulation.
#'
#' Canopy height model based on a triangulation of first returns. Depending on the inputs
#' this function computes a simple Delaunay triangulation of the first returns with a linear
#' interpolation within each triangle. This function also enables the use of the pit-free algorithm
#' developed by Khosravipour et al. (2014), which is based on the computation of a set of classical
#' triangulations at different heights (see reference).
#'
#' @section Use with a \code{LAScatalog}:
#' When the parameter \code{x} is a \link[lidR:LAScatalog-class]{LAScatalog} the function processes
#' the entire dataset in a continuous way using a multicore process. Parallel computing is set
#' by default to the number of core available in the computer. A buffer is required. The user
#' can modify the global options using the function \link{catalog_options}.\cr\cr
#' \code{lidR} support .lax files. Computation speed will be \emph{significantly} improved with a
#' spatial index.
#'
#' @param x A LAS object
#' @param res numeric. Resolution of the canopy height model.
#' @param thresholds numeric. Set of height thresholds. If \code{thresholds = 0} the algorithm
#' is a strict rasterization of the triangulation of the first returns. However, if an array is
#' passed to the function it becomes the Khosravipour et al. pit-free algorithm.
#' @param max_edge numeric. Maximum edge-length of a triangle in the Delaunay triangulation.
#' If a triangle has an edge greater than this value it will be removed. It is used to drive
#' the pit-free algorithm (see reference) and to trim dummy interpolation on non-convex areas.
#' The first number is the value for the classical triangulation (threshold = 0), the second number
#' is the value for the pit-free algorithm for (thresholds > 0). If \code{max_edge = 0} no trimming
#' will be done.
#' @param subcircle numeric. Radius of the circles. To obtain fewer pits the algorithm
#' can replace each return with a circle composed of 8 points before computing the triangulation
#' (see also \link{grid_canopy}).
#' @param filter character. Streaming filter while reading the files (see \link{readLAS}).
#' If the input is a \code{LAScatalog} the function \link{readLAS} is called internally. The
#' user cannot manipulate the lidar data directly but can use streaming filters instead.
#' @return Returns a \code{data.table} of class \code{lasmetrics}, which enables easier
#' plotting and RasterLayer casting.
#' @export
#' @examples
#' LASfile = system.file("extdata", "Tree.laz", package="lidR")
#' las = readLAS(LASfile, Classification = FALSE, Intensity = FALSE, filter = "-drop_z_below 0")
#'
#' # Basic triangulation and rasterization
#' chm1 = grid_tincanopy(las, thresholds = 0, max_edge = 0)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm2 = grid_tincanopy(las, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
#'
#' plot(chm1)
#' plot(chm2)
#' @references Khosravipour, A., Skidmore, A. K., Isenburg, M., Wang, T., & Hussin, Y. A. (2014).
#' Generating pit-free canopy height models from airborne lidar. Photogrammetric Engineering &
#' Remote Sensing, 80(9), 863-872.
grid_tincanopy = function(x, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0, filter = "-keep_first")
{
  UseMethod("grid_tincanopy", x)
}

#' @export
grid_tincanopy.LAS = function(x, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0, filter = "-keep_first")
{
  . <- X <- Y <- Z <- ReturnNumber <- Xgrid <- Ygrid <- NULL

  if (length(thresholds) > 1 & length(max_edge) < 2)
    stop("'max_edge' should contain 2 numbers", call. = FALSE)

  if (!"ReturnNumber" %in% names(x@data))
     stop("No column 'ReturnNumber' found. This field is needed to extract first returns", call. = FALSE)

  if (fast_countequal(x@data$ReturnNumber, 1) == 0)
    stop("No first returns found. Aborted.", call. = FALSE)

  if (length(thresholds) == 1 & thresholds[1] == 0)
    verbose("[Delaunay triangulation of first returns]\n")
  else if (length(thresholds) > 1)
    verbose("[Khosravipour et al. pitfree algorithm]\n")

  # Create the coordinates of interpolation (pixel coordinates)
  verbose("Generating interpolation coordinates...")

  ex = extent(x)
  grid = make_grid(ex@xmin, ex@xmax, ex@ymin, ex@ymax, res)

  # Initialize the interpolated values with NAs
  z = rep(NA, (dim(grid)[1]))

  # Get only first returns and coordinates (nothing else needed)
  verbose("Selecting first returns...")
  cloud = x@data[ReturnNumber == 1, .(X,Y,Z)]

  # subcircle the data
  if (subcircle > 0)
  {
    verbose("Subcircling points...")

    ex = extent(x)
    cloud = subcircled(cloud, subcircle, 8)
    cloud = cloud[between(X, ex@xmin, ex@xmax) & between(Y, ex@ymin, ex@ymax)]
  }

  verbose("Selecting only the highest points within the grid cells...")

  f = function(x,y,z) {
    i = which.max(z)
    return(list(X = x[i], Y = y[i], Z = z[i]))
  }

  by = group_grid(cloud$X, cloud$Y, res)
  cloud = cloud[, f(X,Y,Z), by = by][, Xgrid := NULL][, Ygrid := NULL][]

  # Perform the triangulation and the rasterization (1 loop for classical triangulation, several for Khosravipour)
  i = 1
  for (th in thresholds)
  {
    verbose(paste0("Triangulation pass ", i, " of ", length(thresholds), "..."))
    i =  i+ 1

    if (th == 0)
      edge = max_edge[1]
    else
      edge = max_edge[2]

    cloud = cloud[Z >= th]
    Ztemp = interpolate_delaunay(cloud, grid, edge)
    z = pmax(z, Ztemp, na.rm = T)
  }

  grid[, Z := z][]
  grid = grid[!is.na(Z)]
  as.lasmetrics(grid,res)

  return(grid)
}

#' @export
grid_tincanopy.LAScatalog = function(x, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0, filter = "-keep_first")
{
  oldbuffer <- CATALOGOPTIONS("buffer")

  if (subcircle == 0)
    CATALOGOPTIONS(buffer = res)

  canopy = grid_catalog(x, grid_tincanopy, res, "xyzr", filter, thresholds = thresholds, max_edge = max_edge, subcircle = subcircle)

  CATALOGOPTIONS(buffer = oldbuffer)

  return(canopy)
}
