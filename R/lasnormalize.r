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



#' Substract digital terrain model
#'
#' Substract digital terrain model (DTM) to the LiDAR data to create a dataset
#' normalized with the ground at 0. The digital terrain model can comes from
#' several sources such as external file or own computaion. It can also be computed on the
#' fly.
#'
#' When the paramter dtm is not provided the elevation of the ground is computed for each point
#' of the lidar data with the function \link[lidR:lasterrain]{lasterrain}.
#' The consequence is a more accurate normalisation. Indeed no rasterizaion impling
#' innacuracies is required. This method lead to few negatives values.
#'
#' @param .las a LAS objet
#' @param dtm a RasterLayer from package \link[raster:raster]{raster}. If NULL the function will
#' automatically compute it on the fly. This second solution is more accurate
#' because no rasterization is done (see details).
#' @param ... optionnal parameters for \link[lidR:lasterrain]{lasterrain} if
#' \code{dtm} parameter is NULL.
#' @return A LAS object.
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' # plot(lidar)
#'
#' # --- First possibility: compute the DTM on the fly -----
#'
#' lidar_norm = lasnormalize(lidar)
#'
#' # plot(lidar_norm)
#'
#' # --- Second possibility: read the DTM from a file -----
#'
#'\dontrun{
#' dtm = raster::raster(terrain.tiff)
#'
#' lidar_norm = lidar - dtm # is synonyme with normalize(lidar, dtm)
#'
#' # plot(lidar_norm)
#' }
#'
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
setGeneric("lasnormalize", function(.las, dtm = NULL, ...){standardGeneric("lasnormalize")})

setMethod("lasnormalize", "LAS",
  function(.las, dtm = NULL, ...)
  {
   . <- Z <- Zn <- Xr <- Yr <- NULL

    if(is.null(dtm))
      Zn = lasterrain(.las, .las@data, ...)
    else if(class(dtm)[1] == "RasterLayer")
      Zn = raster::extract(dtm, .las@data[, c("X", "Y"), with = F])
    else
      stop("The terrain model is not a RasterLayer")

    normalized = data.table::copy(.las@data)
    normalized[, Z := round(Z - Zn, 3)][]

    return(LAS(normalized, .las@header))
  }
)

#' Conveniant operator to lasnormalize
#'
#' @param e1 a LAS object
#' @param e2 a RasterLayer
#' @export
setMethod("-", c("LAS", "RasterLayer"),
  function(e1, e2)
  {
      return(lasnormalize(e1,e2))
  }
)