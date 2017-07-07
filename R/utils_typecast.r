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


#' Set the class 'lasmetrics' to a data.frame or a data.table
#'
#' Set the class lasmetrics to a data.frame. Useful when reading data from a file.
#' In this case the data.frame does not have the class lasmetrics and cannot easily be
#' plotted or transformed into a raster
#'
#' @param x A data.frame or a data.table
#' @param res numeric the original resolution
#' @return Nothing. Object is updated in place by reference.
#' @export
#' @family cast
as.lasmetrics = function(x, res)
{
  data.table::setattr(x, "res", res)
  data.table::setattr(x, "class", c("lasmetrics", "data.table", "data.frame"))
}

#' Transform a \code{lasmetrics} object into a spatial \code{RasterLayer} object
#'
#' @param x a \code{lasmetrics} object
#' @param z character. If 3 columns or more, the names of the field to extract. If NULL returns
#' a RasterStack instead of a RasterLayer.
#' @param fun.aggregate Should the data be aggregated before casting? If the table doesn't
#' contain a single observation for each cell, then aggregation defaults to mean value with
#' a message.
#' @param ... Internal use only.
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:grid_canopy]{grid_canopy}
#' \link[lidR:grid_canopy]{grid_canopy}
#' \link[raster:raster]{raster}
#' @return A \link[raster:RasterLayer-class]{RasterLayer} object from package \pkg{raster}
#' or a \link[raster:RasterStack-class]{RasterStack} if there are several layers.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' meanHeight = grid_metrics(lidar, mean(Z))
#' rmeanHeight = as.raster(meanHeight)
#' @method as.raster lasmetrics
#' @importMethodsFrom raster as.raster
#' @export
#' @family cast
as.raster.lasmetrics = function(x, z = NULL, fun.aggregate = mean, ...)
{
  X <- .SD <- flightline <- NULL

  inargs = list(...)

  # Select the column(s)

  if (!is.null(z))
    x = x[, c("X", "Y", z), with = FALSE]

  # Guess the resolution of the raster is the info is missing

  if (is.null(attr(x, "res")))
  {
    verbose("Retrieving the resolution of the raster...")

    dx = x$X %>% unique %>% sort %>% diff
    dy = x$Y %>% unique %>% sort %>% diff
    ts = table(c(dx, dy))

    if (length(ts) == 1)
      res = dx[1]
    else
    {
      res = stats::median(c(dx,dy)) %>% round(2)
      message(paste0("Attribute resolution 'attr(x, \"res\")' not found. Algorithm guessed that resolution was: ", res))
    }

    data.table::setattr(x, "res", res)
  }

  res = attr(x, "res")

  # Aggregate the duplicated entries

  verbose("Checking for duplicated entries...")

  multi = duplicated(x, by = c("X","Y")) %>% sum

  if (multi > 0)
  {
    verbose("Aggregating duplicated entries...")
    x = x[, lapply(.SD, fun.aggregate), by = c("X", "Y")][, flightline := NULL]
    message("Duplicated entries were found and aggregated.")
  }

  # Autocompletion of the grid with NAs

  verbose("Filling empty data with NAs...")

  rx  = range(x$X)
  ry  = range(x$Y)

  grid = expand.grid(X = seq(rx[1], rx[2], res),  Y = seq(ry[1], ry[2], res))
  data.table::setDT(grid)

  data.table::setkeyv(x, c("X", "Y"))
  data.table::setkeyv(grid, c("X", "Y"))

  x = x[grid]

  # Convert to raster

  if (ncol(x) <= 3 && is.null(inargs$spbackend)) # Use the data.table way which is much master (approx 3 times)
  {
    verbose("Casting into raster with dcast...")

    out = data.table::dcast(data = x, formula = X~Y, value.var = names(x)[3])
    out[, X := NULL]
    out = out %>% as.matrix %>% apply(1, rev)

    xmn = min(x$X) - 0.5 * res
    xmx = max(x$X) + 0.5 * res
    ymn = min(x$Y) - 0.5 * res
    ymx = max(x$Y) + 0.5 * res

    return(raster::raster(out, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx))
  }
  else # Use the sp way to get and return a raster stack
  {
    verbose("Casting into raster with sp")

    out = as.data.frame(x)
    sp::coordinates(out) <- ~ X + Y
    sp::gridded(out) <- TRUE   # coerce to SpatialPixelsDataFrame

    if (ncol(out) <= 3)
      return(raster::raster(out))
    else
      return(raster::stack(out))
  }
}

#' Transform a LAS object into a SpatialPointsDataFrame object
#'
#' @param .las an object of class LAS
#' @return An object of class \code{SpatialPointsDataFrame}
#' @export
#' @family cast
as.SpatialPointsDataFrame = function(.las)
{
  . <- X <- Y <- NULL
  stopifnotlas(.las)
  sp::SpatialPointsDataFrame(.las@data[,.(X,Y)], .las@data[, 3:ncol(.las@data), with = F])
}

#' Transform a \code{'lasmetrics'} object into a SpatialPixelsDataFrame object
#'
#' @param .data an object of class \code{lasmetrics}
#' @return An object of class \code{SpatialPixelDataFrame}
#' @export
#' @family cast
as.SpatialPixelsDataFrame = function(.data)
{
  .data = as.data.frame(.data)
  sp::SpatialPixelsDataFrame(.data[c("X","Y")], .data[3:ncol(.data)])
}