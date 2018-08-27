# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

#' @export
#' @rdname grid_canopy
grid_canopy_pitfree = function(las, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0)
{
  UseMethod("grid_canopy_pitfree", las)
}

#' @export
grid_canopy_pitfree.LAS = function(las, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0)
{
  assertive::assert_is_numeric(thresholds)
  assertive::assert_all_are_non_negative(thresholds)
  assertive::assert_is_numeric(max_edge)
  assertive::assert_all_are_non_negative(max_edge)
  assertive::assert_is_a_number(subcircle)
  assertive::assert_all_are_non_negative(subcircle)

  . <- X <- Y <- Z <- ReturnNumber <- Xgrid <- Ygrid <- NULL

  if (length(thresholds) > 1 & length(max_edge) < 2)
    stop("'max_edge' should contain 2 numbers", call. = FALSE)

  if (!"ReturnNumber" %in% names(las@data))
     stop("No column 'ReturnNumber' found. This field is needed to extract first returns", call. = FALSE)

  if (fast_countequal(las@data$ReturnNumber, 1) == 0)
    stop("No first returns found. Aborted.", call. = FALSE)

  if (length(thresholds) == 1 & thresholds[1] == 0)
    verbose("[Delaunay triangulation of first returns]\n")
  else if (length(thresholds) > 1)
    verbose("[Khosravipour et al. pitfree algorithm]\n")

  # Create the coordinates of interpolation (pixel coordinates)
  verbose("Generating interpolation coordinates...")

  layout = make_overlay_raster(las, res, subcircle = subcircle)

  # Initialize the interpolated values with NAs
  z = rep(NA_real_, raster::ncell(layout))

  # Get only first returns and coordinates (nothing else needed)
  verbose("Selecting first returns...")

  cloud = las@data
  if (fast_countequal(las@data$ReturnNumber, 1) < nrow(las@data))
    cloud = las@data[ReturnNumber == 1, .(X,Y,Z)]

  # subcircle the data
  if (subcircle > 0)
  {
    verbose("Subcircling points...")

    ex = raster::extent(las)
    cloud = subcircled(cloud, subcircle, 8)
    cloud = cloud[between(X, ex@xmin, ex@xmax) & between(Y, ex@ymin, ex@ymax)]
  }

  verbose("Selecting only the highest points within the grid cells...")

  cells = raster::cellFromXY(layout, cloud[, .(X,Y)])
  grid  = raster::xyFromCell(layout, 1:raster::ncell(layout))
  grid = data.table::as.data.table(grid)
  data.table::setnames(grid, c("x", "y"), c("X", "Y"))
  cloud = cloud[cloud[, .I[which.max(Z)], by = cells]$V1]

  # Perform the triangulation and the rasterization (1 loop for classical triangulation, several for Khosravipour et al.)
  i = 1
  for (th in thresholds)
  {
    verbose(glue::glue("Triangulation pass {i} of {length(thresholds)}..."))
    i =  i+ 1

    if (th == 0)
      edge = max_edge[1]
    else
      edge = max_edge[2]

    cloud = cloud[Z >= th]

    if (nrow(cloud) > 0)
    {
      Ztemp = interpolate_delaunay(cloud, grid, edge)
      z = pmax(z, Ztemp, na.rm = T)
    }
  }

  if(all(is.na(z)))
    stop("Interpolation failed. Input parameters might be wrong.", call. = FALSE)

  layout[] = z
  names(layout) =  "Z"
  return(layout)
}

#' @export
grid_canopy_pitfree.LAScluster = function(las, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0)
{
  x = readLAS(las, filter = "-keep_first", select = "xyzr")
  if (is.empty(x)) return(NULL)
  bbox = raster::extent(as.numeric(las@bbox))
  metrics = grid_canopy_pitfree(x, res, thresholds, max_edge, subcircle)
  metrics = raster::crop(metrics, bbox)
  return(metrics)
}

#' @export
grid_canopy_pitfree.LAScatalog = function(las, res = 0.5, thresholds =  c(0,2,5,10,15), max_edge = c(0,1), subcircle = 0)
{
  set_select(las) <- "xyzr"
  output          <- catalog_apply2(las, grid_canopy_pitfree, res = res, thresholds = thresholds, max_edge = max_edge, subcircle = subcircle, need_buffer = TRUE, check_alignement = TRUE, drop_null = TRUE)

  # Outputs have been written in files. Return the path to written files
  if (get_output_files(las) != "")  return(unlist(output))

  # Outputs have been return in R objects. Merge the outptus in a single object
  names         <- names(output[[1]])
  factor        <- output[[1]]@data@isfactor
  output        <- do.call(raster::merge, output)
  output@crs    <- las@proj4string
  names(output) <- names
  return(output)
}