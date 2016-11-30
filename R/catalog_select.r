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



#' Select LAS files interactively
#'
#' Select a set of LAS tile from a Catalog using interactively the mouse
#'
#' This function enable the user to select a set of las file from a Catalog clicking on the map of the file with the mouse.
#' The selected files turn red on the plot at the end of the selection.
#' @param x A Catalog object
#' @return A Catalog object
#' @export
#' @examples
#' \dontrun{
#' catalog = lascatalog("<Path to a folder containing a set of .las files>")
#' selectedFiles = catalog_select(catalog)
#' }
#' @seealso
#' \link[lidR:lascatalog]{Catalog}
catalog_select = function(x)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- filename <- NULL

  graphics::plot(x)

  selected = x@headers %$% graphics::identify((Min.X+Max.X)/2, (Min.Y+Max.Y)/2, plot=F)

  x@headers = x@headers[selected,]

  x@headers %$% graphics::rect(Min.X, Min.Y, Max.X, Max.Y, col="red")

  return(x)
}
