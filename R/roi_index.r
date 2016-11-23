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



#' Retrieve the tiles containing ROIs
#'
#' When the user has a set of (x, y) coordinates corresponding to a region of interest (ROI)
#' (a ground inventory for example), he can automatically find in which tiles the lidar data associated
#' to the ROIs from a Catalog are. The algorithm automatically find tiles even for ROIs falling on the edges
#' of one or more tiles.\cr
#' It works only for tiles well organized as a damier. This function is used by \link[lidR:roi_query]{roi_query}.
#' Users do not really need it.
#'
#' @aliases roi_index
#' @param obj A Catalog object
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param r numeric or vector. A radius or a set of radii of the ROI. If only
#' radius is provided (radius2 = NULL) it will extract data falling into a disc.
#' @param r2 numeric or vector. A radius or a set of radii of plots. If radius2
#' is provided, the selection turns into a rectangular ROI. If radius = radius2 it is a square obviouly.
#' @param roinames vector. A set of ROI names
#' @export roi_index
#' @importFrom dplyr select
#' @importFrom data.table data.table :=
setGeneric("roi_index", function(obj, x, y, r, r2 = NULL, roinames = NULL){standardGeneric("roi_index")})

#' @rdname roi_index
setMethod("roi_index", "Catalog",
	function(obj, x, y, r, r2 = NULL, roinames = NULL)
	{
	  tile <- minx <- maxx <- miny <- maxy <- NULL
	  filename <- Min.X <- Max.X <- Min.Y <- Max.Y <- NULL
	  . <- NULL

    nplot = length(x)

	  if(is.null(r2)) r2 = r
	  if(is.null(roinames)) roinames = paste("ROI", 1:nplot, sep="")

	  coord.tiles = obj@headers %>%
            	      dplyr::select(tile = filename,
            	                    minx = Min.X,
            	                    maxx = Max.X,
            	                    miny = Min.Y,
            	                    maxy = Max.Y)

    coord.plot = data.table(roinames, x, y, r, r2)
    coord.plot[,`:=`(maxx = x + r, maxy = y + r2, minx = x - r, miny = y - r2)]

    tiles = lapply(1:nplot, function(i)
    {
      coord = coord.plot[i]
      dplyr::filter(coord.tiles, !(minx >= coord$maxx | maxx <= coord$minx | miny >= coord$maxy | maxy <= coord$miny))$tile
    })

    coord.plot[, tiles := list(tiles)]
    coord.plot[, c("maxx", "maxy", "minx", "miny") := NULL]

    return(coord.plot[])
	}
)