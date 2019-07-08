# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2019 Jean-Romain Roussel
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

#' A set of boolean tests on objects
#'
#' \code{is.empty} tests if a \code{LAS} object is a point cloud with 0 points.\cr
#' \code{is.overlapping} tests if a \code{LAScatalog} has overlapping tiles.\cr
#' \code{is.indexed} tests if the points of a \code{LAScatalog} are indexed with \code{.lax} files.\cr
#' \code{is.algorithm} tests if an object is an algorithm of the lidR package.\cr
#' \code{is.parallelised} tests if an algorithm of the lidR package is natively parallelised with OpenMP.
#' Returns TRUE if the algorithm is at least partially parallelised i.e. if some portion of the code is
#' computed in parallel.
#'
#' @param las A \code{LAS} object.
#' @param x Any R object.
#' @param catalog A \code{LAScatalog} object.
#' @param algorithm An \code{algorithm} object.
#'
#' @return TRUE or FALSE
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#' is.empty(las)
#'
#' las = new("LAS")
#' is.empty(las)
#'
#' f <- lmf(2)
#' is.parallelised(f)
#'
#' g <- pitfree()
#' is.parallelised(g)
#'
#' ctg <- readLAScatalog(LASfile)
#' is.indexed(ctg)
#' @export
#' @rdname is
#' @name is
NULL

#' @rdname is
#' @export
is.empty <- function(las)
{
  stopifnotlas(las)
  return(nrow(las@data) == 0L)
}

#' @rdname is
#' @export
is.overlapping = function(catalog)
{
  contour       <- rgeos::gUnaryUnion(catalog)
  actual_area   <- round(contour@polygons[[1]]@area, 4)
  measured_area <- round(area(catalog), 4)
  return(actual_area < measured_area)
}

#' @rdname is
#' @export
is.indexed = function(catalog)
{
  laxfiles <- paste0(tools::file_path_sans_ext(catalog@data$filename), ".lax")
  return(!any(!file.exists(laxfiles)))
}

#' @rdname is
#' @export
is.algorithm = function(x)
{
  return(is(x, "lidR") && is(x, "Algorithm"))
}

#' @rdname is
#' @export
is.parallelised = function(algorithm)
{
  if (!is.algorithm(algorithm))
    stop("This function only applies to algorithms from the lidR package")

  return(is(algorithm, "OpenMP"))
}
