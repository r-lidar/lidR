# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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

Cluster <- function(center, width, height, buffer, shape, files, name)
{
  return(new("LAScluster", center, width, height, buffer, shape, files, name, ""))
}

LAScluster <- function(center, width, height, buffer, shape, files, name, proj4string = sp::CRS())
{
  return(new("LAScluster", center, width, height, buffer, shape, files, name, "", proj4string))
}


setMethod("show", "LAScluster", function(object)
{
  cat("class   : LAScluster\n")
  cat("name    :", object@name, "\n")
  cat("center  :", object@center$x, ",", object@center$y, "\n")
  cat("extent  :", object@bbox[1], ",", object@bbox[3], ",", object@bbox[2], ",", object@bbox[4], "(xmin, xmax, ymin, ymax)\n")
  cat("extent+ :", object@bbbox[1], ",", object@bbbox[3], ",", object@bbbox[2], ",", object@bbbox[4], "(xmin, xmax, ymin, ymax)\n")
  cat("size    :", object@width, "x", object@height, "\n")
  cat("files   :", basename(object@files), "\n")
  cat("filter  :", object@filter, "\n")
})