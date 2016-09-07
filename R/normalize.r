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
#' several sources such as external file or own computaion.
#'
#' @param las a LAS objet
#' @param dtm a digital terrain model. It can be a RasterLayer from package
#' \link[raster:raster]{raster} or a DTM from computed with
#' \link[lidR:grid_terrain]{grid_terrain}. If NULL the function will automatocally
#' compute it with \link[lidR:grid_terrain]{grid_terrain}.
#' @param ... optionnal parameters for \link[lidR:grid_terrain]{grid_terrain} if
#' \code{dtm} parameter is NULL.
#' @return A LAS object.
#' @export
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' plot(lidar)
#'
#' # --- First possibility: read the DTM from a file -----
#' DTMfile <- system.file("extdata", "Topography.tif", package="lidR")
#' dtm = raster::raster(DTMfile)
#'
#' lidar_norm = lidar - dtm # is synonyme with normalize(lidar, dtm)
#'
#' plot(lidar_norm)
#'
#' # --- Second possibility: compute the DTM with grid_terrain -----
#'
#' # Linear interpolation is fast, linear = FALSE for spline interpolation
#' dtm = grid_terrain(lidar, linear = TRUE)
#'
#' lidar_norm = lidar - dtm
#'
#' plot(lidar_norm)
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @importFrom data.table setnames
#' @importFrom dplyr left_join mutate select filter
#' @importFrom plyr round_any
normalize = function(las, dtm = NULL, ...)
{
  Z <- Zn <- X <- Y <- Z <- NULL

  if(is.null(dtm))
    dtm = las %>% grid_terrain(...)
  else if(class(dtm)[1] == "RasterLayer")
    dtm = as.DTM(dtm)
  else if(class(dtm)[1] != "DTM")
    stop("The terrain model is neither a DTM nor a RasterLayer")

  if(!identical(c("x", "y", "z"), names(dtm)) | !is.list(dtm) | !is.matrix(dtm$z))
    stop("Internal representation of the terrain model is not correct.")

  xmin = min(dtm$x)
  ymin = min(dtm$y)
  res  = attr(dtm, "res")

  Xr = plyr::round_any(las@data$X-0.5*res-xmin, res)+xmin
  Yr = plyr::round_any(las@data$Y-0.5*res-ymin, res)+ymin
  las@data$Xr = Xr
  las@data$Yr = Yr

  x = match(Xr, dtm$x)
  y = match(Yr, dtm$y)

  normalized = las@data[, dtm$z[match(Xr, dtm$x), match(Yr, dtm$y)], by=c("Xr","Yr")]
  data.table::setnames(normalized, c("Xr", "Yr", "Zn"))

  data = dplyr::left_join(las@data, normalized, by=c("Xr", "Yr")) %>%
    dplyr::mutate(Z = Z-Zn) %>%
    dplyr::select(-Xr, -Yr, -Zn) %>%
    dplyr::filter(!is.na(Z))

  class(data) = c("data.table", "data.frame")

  return(LAS(data, las@header))
}

#' Conveniant operator to normalize
#'
#' @param e1 a LAS object
#' @param e2 a digital terrain model
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' plot(lidar)
#'
#' # --- First possibility: read the DTM from a file -----
#' DTMfile <- system.file("extdata", "Topography.tif", package="lidR")
#' dtm = raster::raster(DTMfile)
#'
#' lidar_norm = lidar - dtm # is synonyme with normalize(lidar, dtm)
#'
#' plot(lidar_norm)
#'
#' # --- Second possibility: compute the DTM with grid_terrain -----
#'
#' # Linear interpolation is fast, linear = FALSE for spline interpolation
#' dtm = grid_terrain(lidar, linear = TRUE)
#'
#' lidar_norm = lidar - dtm
#'
#' plot(lidar_norm)
#' @export
setMethod("-", "LAS",
  function(e1, e2)
  {
      return(normalize(e1,e2))
  }
)