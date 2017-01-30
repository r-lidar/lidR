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

convex_hull = function(x, y)
{
	data = data.frame(x,y)
	ch <- grDevices::chull(x,y)
	coords <- data[c(ch, ch[1]), ]
	rownames(coords) = NULL
	return(coords)
}

polygon_area = function(x, y)
{
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!is.numeric(x) || !is.numeric(y) ) stop("Arguments 'x' and 'y' must be real")
  if (length(x) != length(y)) stop("Argument 'x' and 'y' must be of same size")

	area = 0;
	j = length(x)

	for (i in 1:j)
	{
		area = area + (x[j]+x[i])*(y[j]-y[i]);
		j = i;
	}

	area  = abs(area*0.5)

	return(area)
}

area = function(x, y)
{
  hull = convex_hull(x, y)
  area = polygon_area(hull$x, hull$y)
  area = round(area,1)
  return(area)
}