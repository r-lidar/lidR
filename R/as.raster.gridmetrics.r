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



#' Tranform a grid_metrics object into a spatial raster object
#'
#' @param x a grid_metrics object
#' @param z character. The field to plot. If NULL, autodetect
#' @param \dots other parameters for \link[data.table:dcast]{dcast}
#' @seealso
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:grid_canopy]{grid_canopy}
#' \link[data.table:dcast]{dcast}
#' \link[raster:raster]{raster}
#' @return A RasterLayer object from package  \link[raster:raster]{raster}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' meanHeight = grid_metrics(lidar, 20, mean(Z))
#' mtx = as.raster(meanHeight)
#' @importFrom data.table dcast
#' @method as.raster gridmetrics
#' @importFrom magrittr %>%
#' @importMethodsFrom raster as.raster
#' @export
as.raster.gridmetrics = function(x, z = NULL, ...)
{
  X <- NULL

  inargs <- list(...)


  multi = duplicated(x, by = c("X","Y")) %>% sum

  if(multi > 0 & is.null(inargs$fun.aggregate))
    lidRError("GDM2", number = multi, behavior = message)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM3")
    else
      z = names(x)[3]
  }

  rx = range(x$X)
  ry = range(x$Y)
  x  = x[, c("X", "Y", z), with=F]

  grid = expand.grid(X = seq(rx[1], rx[2], 20),  Y = seq(ry[1], ry[2], 20))
  grid = data.table::setDT(grid)

  setkeyv(x, c("X", "Y"))
  setkeyv(grid, c("X", "Y"))

  data = x[grid]

  if(is.null(inargs$fun.aggregate))
    out = data.table::dcast(data = data, formula = X~Y, value.var=z, fun.aggregate = mean, ...)
  else(is.null(inargs$fun.aggregate))
    out = data.table::dcast(data = data, formula = X~Y, value.var=z, ...)

  out[, X := NULL]
  mx = out %>% as.matrix
  mx = apply(mx, 1, rev)

  layer = raster::raster(mx, xmn = min(x$X), xmx = max(x$X), ymn = min(x$Y), ymx = max(x$Y))

  return(layer)
}
