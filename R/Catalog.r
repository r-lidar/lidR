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



#' Build a Catalog object
#'
#' Methods to creates a \code{Catalog} object from a folder name
#'
#' A catalog is the representation of a set of las files. A computer cannot load all
#' the data at the same time. A catalog is a simple way to manage all the file sequentially
#' reading only the headers.
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Extra parameters to \link[base:list.files]{list.files}. Typically `recursive = TRUE`.
#' @seealso
#' \link[lidR:Catalog]{Catalog-class}
#' \link[lidR:plot.Catalog]{plot}
#' \link[lidR:process_parallel]{process_parallel}
#' \link[lidR:roi_query]{roi_query}
#' @return A Catalog object
#' @export Catalog
#' @importFrom methods new
Catalog <- function(folder, ...) {return(new("Catalog", folder, ...))}