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

round_any <- function(x, accuracy)
{
  roundc(x / accuracy) * accuracy
}

make_grid = function(xmin, xmax, ymin, ymax, res, start = c(0,0))
{
  xo = seq(f_grid(xmin, res, start[1]), f_grid(xmax, res, start[1]), res)
  yo = seq(f_grid(ymin, res, start[2]), f_grid(ymax, res, start[2]), res)

  grid = expand.grid(X = xo, Y = yo)
  data.table::setDT(grid)

  return(grid)
}

make_overlay_raster = function(las, res, start = c(0,0), subcircle = 0)
{
  if (is(res, "RasterLayer"))
  {
    resolution = raster::res(res)
    if (resolution[1] !=  resolution[2]) stop("Rasters with different x y resolutions are not supported")
    return(res)
  }

  bbox      <- raster::extent(las) + 2 * subcircle
  bbox@xmin <- round_any(bbox@xmin - 0.5 * res - start[1], res) + start[1]
  bbox@xmax <- round_any(bbox@xmax - 0.5 * res - start[1], res) + res + start[1]
  bbox@ymin <- round_any(bbox@ymin - 0.5 * res - start[2], res) + start[2]
  bbox@ymax <- round_any(bbox@ymax - 0.5 * res - start[2], res) + res + start[2]
  layout    <- suppressWarnings(raster::raster(bbox, res = res, crs = las@proj4string))
  return(layout)
}

merge_rasters = function(output)
{
  # Outputs have been returned in R objects. Merge the outputs in a single object
  if (length(output) > 1)
  {
    names         <- names(output[[1]])
    factor        <- output[[1]]@data@isfactor
    output        <- do.call(raster::merge, output)
    names(output) <- names
    if (is(output, "RasterBrick")) colnames(output@data@values) <- names
  }

  return(output)
}

build_vrt = function(output, vrt)
{
  if (!requireNamespace("gdalUtils", quietly = TRUE))
  {
    message("'gdalUtils' package is needed to build a virtual raster mosaic. Return the list of written files instead.")
    return(unlist(output))
  }

  output <- unlist(output)
  folder <- dirname(output[1])
  file   <- paste0("/", vrt, ".vrt")
  vrt    <- paste0(folder, file)
  gdalUtils::gdalbuildvrt(output, vrt)
  return(raster::stack(vrt))
}


group_grid = function(x, y, res, start = c(0,0))
{
  xgrid = f_grid(x, res, start[1])
  ygrid = f_grid(y, res, start[2])

  return(list(Xgrid = xgrid, Ygrid = ygrid))
}

group_grid_3d = function(x, y, z, res, start = c(0,0,0))
{
  xgrid = f_grid(x, res[1], start[1])
  ygrid = f_grid(y, res[1], start[2])
  zgrid = f_grid(z, res[2], start[3])

  return(list(Xgrid = xgrid, Ygrid = ygrid, Zgrid = zgrid))
}

f_grid = function(x, res, start)
{
  round_any(x - 0.5 * res - start, res) + 0.5 * res + start
}

verbose = function(...)
{
  if (getOption("lidR.verbose"))
    cat(..., "\n")
}

dummy_las = function(n, seeds = c)
{
  set.seed(1)
  X = stats::runif(n, 0, 100)
  set.seed(2)
  Y = stats::runif(n, 0, 100)
  set.seed(3)
  Z = c(stats::runif(0.8*n, 0, 25), rep(0, 0.2*n))
  Classification = as.integer(c(rep(1, 0.8*n), rep(2, 0.2*n)))
  Intensity = sample(10:50, n, TRUE)
  ReturnNumber    = as.integer(rep(c(1,1,1,2,3,1,2,1,2,1), n/10))
  NumberOfReturns = as.integer(rep(c(1,1,3,3,3,2,2,2,2,1), n/10 ))

  dt = data.table::data.table(X, Y, Z, Classification, Intensity, ReturnNumber, NumberOfReturns)
  las = suppressWarnings(LAS(dt))

  return(las)
}

subcircled = function(dt, r, n)
{
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

  return(dt[, f(X, Y, Z, px, py), by = rownames(dt)][, rownames := NULL][])
}

coordinates = function(las)
{
  DT <- las@data
  X  <- DT[["X"]]
  Y  <- DT[["Y"]]
  return(data.frame(X,Y))
}

coordinates3D = function(las)
{
  DT <- las@data
  X  <- DT[["X"]]
  Y  <- DT[["Y"]]
  Z  <- DT[["Z"]]
  return(data.frame(X,Y,Z))
}

