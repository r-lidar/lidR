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



#' Local canopy surface model
#'
#' local_maximum is a reimplementation of grid_canopy with the local maximum algorithm
#' for local analysis (e.g. in a raster), avoiding side effects of the algorithm used to
#' produce the grid. It is only useful to compute your own metric in a function.
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param z vector of z coordinates
#' @param res numeric. The size of the cells
#' @return A matrix which is the raster of the canopy surface model
#' @export local_maximum
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' myClosureMetrics = function(x, y, z)
#' {
#'    canopy = local_maximum(x,y,z, 2)
#'
#'    CC2  = canopy_closure(canopy, 2)
#'    CC10 = canopy_closure(canopy, 10)
#'    CC20 = canopy_closure(canopy, 20)
#'
#'    return(list(CC2 = CC2, CC10 = CC10, CC20 = CC20))
#' }
#'
#' closures = grid_metrics(lidar, 20, myClosureMetrics(X,Y,Z))
#' plot(closures, "CC2")
#' plot(closures, "CC10")
#' plot(closures, "CC20")
#' @seealso
#' \link[lidR:canopy_closure]{canopy_closure}
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:cloud_metrics]{cloud_metrics}
#' @importFrom plyr round_any
#' @importFrom data.table data.table dcast
local_maximum = function(x,y,z, res)
{
  X <- Y <- Z <- NULL

  xr = x - min(x)
  xr = plyr::round_any(xr-0.5*res, res)+0.5*res

  yr = y - min(y)
  yr = plyr::round_any(yr-0.5*res, res)+0.5*res

  frst = data.table(Z = z)

  canopy = frst[, list(Z = max(Z)), by = list(X = xr,Y = yr)]

  out = data.table::dcast(canopy, X~Y, value.var = "Z")
  xnames = out$X
  out[, X := NULL]
  out %<>% as.matrix
  rownames(out) = xnames

  return(out)
}
