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


#' Set the class gridmetrics to a data.frame or a data.table
#'
#' Set the class gridmetrics to a data.frame. Useful when reading data from a file.
#' In this case the data.frame does not have the class gridmetrics and cannot easly be
#' plotted or transformed into a raster
#'
#' @param x A data.frame or a data.table
#' @param res numeric the original resolution
#' @export as.gridmetrics
as.gridmetrics = function(x, res)
{
  data.table::setDT(x)
  attr(x, "class") = c("gridmetrics", attr(x, "class"))
  attr(x, "res")   = res
  return(x)
}
