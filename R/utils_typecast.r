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
  data.table::setDT(x)
  data.table::setattr(x, "class", c("lasmetrics", attr(x, "class")))
  data.table::setattr(x, "res", res)
}

#' Transform a \code{lasmetrics} object into a spatial \code{RasterLayer} object
#'
#' @param x a \code{lasmetrics} object
#' @param z character. If 3 columns or more, the field to extract. If NULL return a RasterStack.
#' @param \dots unused
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
as.raster.lasmetrics = function(x, z = NULL, ...)
{
  if (!is.null(z))
    x = x[, c("X", "Y", z), with = FALSE]

  # Guess the resolution of the raster is the info is missing
  if (is.null(attr(x, "res")))
  {
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

  # raster or sp package cannot deal when the grid has empty column/rows
  # autocompletion with NAs
  rx  = range(x$X)
  ry  = range(x$Y)

  grid = expand.grid(X = seq(rx[1], rx[2], res),  Y = seq(ry[1], ry[2], res))
  data.table::setDT(grid)

  data.table::setkeyv(x, c("X", "Y"))
  data.table::setkeyv(grid, c("X", "Y"))

  data = x[grid]

  # Convert to raster
  data.table::setDF(data)
  sp::coordinates(data) <- ~ X + Y
  sp::gridded(data) <- TRUE   # coerce to SpatialPixelsDataFrame

  if (ncol(x) <= 3)
    raster <- raster::raster(data)
  else
    raster <- raster::stack(data)

  return(raster)
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


# OLD CODES

# as.matrix.lasmetrics = function(x, z = NULL, ...)
# {
#   X <- Y <- NULL
#
#   inargs <- list(...)
#
#   multi = duplicated(x, by = c("X","Y")) %>% sum
#
#   if (multi > 0 & is.null(inargs$fun.aggregate))
#     lidRError("GDM2", number = multi, behaviour = warning)
#
#   if (is.null(z))
#   {
#     if (length(names(x)) > 3)
#       lidRError("GDM3")
#     else
#       z = names(x)[3]
#   }
#
#   res = attr(x, "res")
#
#   rx  = range(x$X)
#   ry  = range(x$Y)
#   x   = x[, c("X", "Y", z), with = F]
#
#   grid = expand.grid(X = seq(rx[1], rx[2], res),  Y = seq(ry[1], ry[2], res))
#   data.table::setDT(grid)
#
#   data.table::setkeyv(x, c("X", "Y"))
#   data.table::setkeyv(grid, c("X", "Y"))
#
#   data = x[grid]
#
#   if (is.null(inargs$fun.aggregate))
#     out = data.table::dcast(data = data, formula = X~Y, value.var = z, fun.aggregate = mean, ...)
#   else
#     out = data.table::dcast(data = data, formula = X~Y, value.var = z, ...)
#
#   out[, X := NULL]
#   mx = out %>% as.matrix
#
#   return(mx)
# }

# as.raster.lasmetrics = function(x, z = NULL, ...)
# {
#   if (is.null(attr(x, "res")))
#   {
#     dx = x$X %>% unique %>% sort %>% diff
#     dy = x$Y %>% unique %>% sort %>% diff
#     ts = table(c(dx, dy))
#
#     if (length(ts) == 1)
#       res = dx[1]
#     else
#     {
#       res = stats::median(c(dx,dy)) %>% round(2)
#       message(paste0("Attribute resolution 'attr(x, \"res\")' not found. Algorithm guessed that resolution was: ", res))
#     }
#
#     data.table::setattr(x, "res", res)
#   }
#   else
#     res = attr(x, "res")
#
#   mx  = as.matrix(x, z)
#   mx  = apply(mx, 1, rev)
#
#   layer = raster::raster(mx, xmn = min(x$X) - 0.5*res, xmx = max(x$X) + 0.5*res, ymn = min(x$Y) - 0.5*res, ymx = max(x$Y) + 0.5*res)
#
#   return(layer)
# }