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



#' Canopy Closure
#'
#' canopy_closure computes estimates of canopy closure using a canopy surface model.
#' Output values for cover estimates range from 0.0 to 100.0 percent. Canopy closure
#' is defined as the number of cells over a specified height threshold divided by
#' the total cells of the canopy.
#'
#' @param x numeric matrix. A canopy surface model
#' @param threshold numeric. Threshold value
#' @param na.rm logical. If TRUE NA values in the number of cells are not counted (useful for circular plots, for example)
#' @return A numeric percentage value between 0 and 100
#' @seealso
#' \link[lidR:local_maximum]{local_maximum}
#' \link[lidR:grid_metrics]{grid_metrics}
#' \link[lidR:cloud_metrics]{cloud_metrics}
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
#' @export canopy_closure
canopy_closure = function(x, threshold, na.rm = TRUE)
{
  if(!is.matrix(x))
    stop("'x' is not a correct type")

  num  = sum(x > threshold, na.rm=TRUE)

  if(na.rm)
    over = length(x[!is.na(x)])
  else
    over = length(x)

  CC  = num/over*100

  return(CC)
}
