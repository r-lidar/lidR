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


as.lasmetrics = function(x, res)
{
  assertive::assert_is_a_number(res)
  assertive::assert_all_are_positive(res)
  data.table::setattr(x, "res", res)
  data.table::setattr(x, "class", c("lasmetrics", "data.table", "data.frame"))
}

as.raster.lasmetrics = function(x, z = NULL, fun.aggregate = mean, ...)
{
  X <- Y <- . <- SD <- flightline <- NULL

  inargs = list(...)

  # Select the column(s)

  if (!is.null(z))
    x = x[, c("X", "Y", z), with = FALSE]

  # Guess the resolution of the raster is the info is missing

  if (is.null(attr(x, "res")))
  {
    verbose("Retrieving the resolution of the raster...")

    dx = diff(sort(unique(x$X)))
    dy =  diff(sort(unique(x$Y)))
    ts = table(c(dx, dy))

    if (length(ts) == 1)
      res = dx[1]
    else
    {
      res = round(stats::median(c(dx,dy)), 2)
      message(glue("Attribute resolution 'attr(x, \"res\")' not found. Algorithm guessed that resolution was: {res}"))
    }

    data.table::setattr(x, "res", res)
  }

  res = attr(x, "res")

  # Aggregate the duplicated entries

  verbose("Checking for duplicated entries...")

  multi = sum(duplicated(x, by = c("X","Y")))

  if (multi > 0)
  {
    verbose("Aggregating duplicated entries...")
    x = x[, lapply(.SD, fun.aggregate), by = c("X", "Y")][, flightline := NULL]
    message("Duplicated entries were found and aggregated.")
  }

  # Convert to raster

  if (ncol(x) <= 3 && is.null(inargs$spbackend))
  {
    verbose("Casting into RasterLayer")

    hres = 0.5*res

    xmin = min(x$X)
    xmax = max(x$X)
    ncol = (xmax - xmin)/res

    ymin = min(x$Y)
    ymax = max(x$Y)
    nrow = (ymax - ymin)/res

    r <- raster::raster(nrow=nrow, ncol=ncol, xmn=xmin-hres, xmx=xmax+hres, ymn=ymin-hres, ymx=ymax+hres, res = c(res,res))
    cells <- raster::cellFromXY(r, x[, .(X,Y)])
    suppressWarnings(r[cells] <- x[[3]])
    names(r) <- names(x)[3]

    return(r)
  }
  else # Use the sp way to get and return a raster stack (slower)
  {
    verbose("Filling empty data with NAs...")

    rx  = range(x$X)
    ry  = range(x$Y)

    grid = expand.grid(X = seq(rx[1], rx[2], res),  Y = seq(ry[1], ry[2], res))
    data.table::setDT(grid)

    data.table::setkeyv(x, c("X", "Y"))
    data.table::setkeyv(grid, c("X", "Y"))

    x = x[grid]

    verbose("Casting into RasterStack with sp")

    out = as.data.frame(x)
    sp::coordinates(out) <- ~ X + Y
    sp::gridded(out) <- TRUE   # coerce to SpatialPixelsDataFrame

    if (ncol(out) == 1)
      return(raster::raster(out))
    else
      return(raster::stack(out))
  }
}

#' Transform a lidR object into sp object
#'
#' LAS, LAScatalog, lasmetrics are transformed respectively into SpatialPointsDataFrame,
#' SpatialPolygonsDataFrame, SpatialPixelsDataFrame
#'
#' @param x an object from the lidR package
#' @return An object from sp
#' @export
#' @family cast
as.spatial = function(x)
{
  UseMethod("as.spatial", x)
}

#' @export
as.spatial.LAS = function(x)
{
  ..coords <- NULL
  coords = c("X", "Y")
  sp::SpatialPointsDataFrame(x@data[,..coords], x@data[, -..coords], proj4string = x@crs)
}

#' @export
as.spatial.lasmetrics = function(x)
{
  ..coords <- NULL
  coords <- c("X", "Y")
  points <- as.data.frame(x@data[,..coords])
  data   <- as.data.frame(x@data[,-..coords])
  sp::SpatialPixelsDataFrame(points, data)
}

#' @export
as.spatial.LAScatalog = function(x)
{
  res <- new("SpatialPolygonsDataFrame")
  res@bbox <- x@bbox
  res@proj4string <- x@proj4string
  res@plotOrder <- x@plotOrder
  res@data <- x@data
  res@polygons <- x@polygons
  return(res)
}