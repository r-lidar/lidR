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



#' Digital Terrain Model
#'
#' Interpol ground points using linear interpolation or spline interpolation and
#' create a digital terrain model (DTM). This function rely on the
#' \link[akima:interp]{interp} function from package \code{akima}.
#'
#' @param obj LAS objet
#' @param res resolution
#' @param linear using linear or spline interpolation. Default is spline (slower
#' but more realistic).
#' @param ... optionnal parameters for \link[akima:interp]{interp}.
#'
#' @return A \code{list} containing the elevation for each cell of the
#' output grid. The list has the class "DTM" enabling to easily deal with it in
#' the \code{lidR} package.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' plot(lidar)
#'
#' # Linear interpolation is fast, linear = FALSE for spline interpolation
#' dtm = grid_terrain(lidar, linear = TRUE)
#'
#' plot(dtm)
#'
#' \dontrun{
#' }
#' @seealso
#' \link[akima:interp]{interp}
#' \link[lidR:normalize]{normalize}
#' @importFrom akima interp
setGeneric("grid_terrain", function(obj, res = 1, linear = F, ...){standardGeneric("grid_terrain")})

#' @rdname grid_terrain
setMethod("grid_terrain", "LAS",
  function(obj, res = 1 , linear = F, ...)
  {
    X <- Y <- Z <- NULL

    ex = extent(obj)
    xo = seq(floor(ex@xmin),ceiling(ex@xmax), res)
    yo = seq(floor(ex@ymin),ceiling(ex@ymax), res)

    ground = obj %>% getGround
    dtm = ground$data %$% akima::interp(X, Y, Z, xo = xo, yo = yo, linear = linear, ...)

    class(dtm) = c("DTM", "list")
    attr(dtm, "res") = res

    return(dtm)
  }
)
