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



#' Compute the convex hull of set of points
#'
#' Computes the convex hull of the set of points specified.
#'
#' @param x coordinates vectors of points
#' @param y coordinates vectors of points
#' @return A data frame with the coordinates of the unique points lying on the convex hull, in clockwise order. The first point is repeated to close the hull.
#' @examples
#' x = runif(20)
#' y = runif(20)
#' hull = convex_hull(x,y)
#'
#' plot(x,y)
#' lines(hull)
#' @seealso \link[grDevices:chull]{chull}
#' @export convex_hull
#' @importFrom grDevices chull
convex_hull = function(x, y)
{
	data = data.frame(x,y)
	ch <- grDevices::chull(x,y)
	coords <- data[c(ch, ch[1]), ]
	rownames(coords) = NULL
	return(coords)
}
