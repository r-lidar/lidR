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



#  ========= IN DEVELOPPEMENT =========


#' Extract inventory from a set of tiles
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plot automatically.
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plot automatically. The algorithm automatically extracts
#' plots falling on the edges of tiles.
#'
#' @aliases extractGroundInventory
#' @param obj A Catalog object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radii of plots
#' @param buffer numeric. A buffer value to extend the search range
#' @return A list of LAS objects
#' @export extractGroundInventory
#' @importFrom dplyr group_by summarise ungroup progress_estimated
#' @importFrom magrittr %>% %<>%
setGeneric("extractGroundInventory", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("extractGroundInventory")})

#' @rdname extractGroundInventory
setMethod("extractGroundInventory", "Catalog",
  function(obj, plotnames, x, y, radius, buffer = 2)
  {
    tile1 <- tile2 <- tile3 <- tile4 <- NULL

    nplot = length(plotnames)

    tilesForPlots = obj %>% retrieveInventoryTiles(plotnames, x, y, radius, buffer)

    tilesForPlots %<>% dplyr::group_by(tile1, tile2, tile3, tile4) %>%
      dplyr::summarise(plotnames = list(plotnames), X = list(X), Y = list(Y), radius = list(radius))
    output = vector("list", nplot)

    cat("\nExtracting plot inventories...\n")

    p <- dplyr::progress_estimated(nplot)

    k = 1

    for(i in 1:dim(tilesForPlots)[1])
    {
      line   = tilesForPlots[i]

      file1  = line$tile1
      file2  = line$tile2
      file3  = line$tile3
      file4  = line$tile4

      files  = c(file1, file2, file3, file4)
      files  = files[!is.na(files)]

      names  = line$plotnames[[1]]
      X      = line$X[[1]]
      Y      = line$Y[[1]]
      radius = line$radius[[1]]

      lidar  = readLAS(files)

      for(j in 1:length(names))
      {
        output[[k]] = clipCircle(lidar, X[j], Y[j], radius[j])
        k = k+1
        p$tick()$print()
      }
    }

    names(output) = plotnames

    return(output)
  }
)
