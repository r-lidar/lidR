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



#' Plot an object of class gridmetrics in 3D
#'
#' @param x A data.frame or data.table of class gridmetrics.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param \dots Other parameters for \link[rgl:surface3d]{surface3}
#' @seealso
#' \link[lidR:gridmetrics]{gridmetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[rgl:surface3d]{surface3d}
#' \link[lidR:plot.gridmetrics]{plot2d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' gridmetrics(lidar, 2, max(Z)) %>% plot3d
#'
#' # Mean height with 400 m^2 cells
#' gridmetrics(lidar, 20, mean(Z)) %>% plot3d
#' @importFrom rgl surface3d
#' @export
plot3d = function(x, z = NULL, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM1")
    else
      z = names(x)[3]
  }

  mtx = as.matrix(x, z)

  if(is.null(inargs$col))
	{
  	zlim       <- range(mtx, na.rm = TRUE)
  	zlen       <- zlim[2] - zlim[1] + 1
  	colorlut   <- height.colors(zlen)
  	inargs$col <- colorlut[ mtx-zlim[1]+1 ]
  }

  do.call(rgl::surface3d, c(list(x=rownames(mtx), y=colnames(mtx), z=mtx), inargs))
}
