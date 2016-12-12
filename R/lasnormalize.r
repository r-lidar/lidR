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



#' Subtract digital terrain model
#'
#' Subtract digital terrain model (DTM) from the LiDAR data to create a dataset
#' normalized with the ground at 0. The digital terrain model can originate from
#' several sources e.g. from an external file or computed by the user. It can also be computed on the
#' fly.
#'
#' @param .las a LAS objet
#' @param dtm a RasterLayer object from package \link[raster:raster]{raster}. If NULL the function will
#' automatically compute it on the fly using the function \link[lidR:grid_terrain]{grid_terrain}.
#' @param ... optional parameters for \link[lidR:grid_terrain]{grid_terrain} if
#' \code{dtm} parameter is NULL.
#' @return A LAS object.
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' plot(lidar)
#'
#' # --- First possibility: compute the DTM on the fly -----
#'
#' lidar_norm = lasnormalize(lidar, method = "knnidw")
#'
#' plot(lidar_norm)
#'
#' \dontrun{
#' # --- Second possibility: read the DTM from a file -----
#'
#' dtm = raster::raster(terrain.tiff)
#'
#' lidar_norm = lidar - dtm # is synonymous with lasnormalize(lidar, dtm)
#'
#' plot(lidar_norm)
#' }
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
lasnormalize = function(.las, dtm = NULL, ...)
{
  . <- Z <- Zn <- X <- Y <- NULL

  stopifnotlas(.las)

  normalized = LAS(data.table::copy(.las@data), .las@header)

  if(is.null(dtm))
  {
    normalized = normalized - grid_terrain(normalized, ...)
    return(normalized)
  }
  else if(is(dtm, "RasterLayer"))
  {
    lasclassify(normalized, dtm, "Zn")

    isna = is.na(normalized$Zn)
  	if(sum(isna) > 0)
	    warning(paste0(sum(isna), " points with NA elevation points found and removed."), call. = F)

    normalized = lasfilter(normalized, !isna)
    normalized@data[, Z := round(Z - Zn, 3)][, Zn := NULL][]

    gc()

    return(normalized)
  }
  else
    stop("The terrain model is not a RasterLayer", call. = F)
}

#' Convenient operator to lasnormalize
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
