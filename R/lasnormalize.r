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
#' Subtract digital terrain model (DTM) from LiDAR data to create a dataset
#' normalized with the ground at 0. The DTM can originate from
#' several sources e.g. from an external file or computed by the user. It can also be computed
#' on the fly. In this case the algorithm does not use rasterized data and each point is
#' interpolated. There is no inacuracy due to the discretization of the terrain (but it is
#' slower).
#'
#' @param .las a LAS object
#' @param dtm a \link[raster:raster]{RasterLayer} or a \code{lasmetrics} object computed with
#' \link[lidR:grid_terrain]{grid_terrain}.
#' @param method character. Used if \code{dtm = NULL}. Can be \code{"knnidw"},
#' \code{"delaunay"} or \code{"kriging"} (see \link{grid_terrain} for more details)
#' @param k numeric. Used if \code{dtm = NULL}. Number of k-nearest neighbours when the selected
#' method is either \code{"knnidw"} or \code{"kriging"}
#' @param model Used if \code{dtm = NULL}. A variogram model computed with \link[gstat:vgm]{vgm}
#' when the selected method is \code{"kriging"}. If NULL it performs an ordinary or weighted least
#' squares prediction.
#' @return A \code{LAS} object
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' plot(las)
#'
#' # --- First option: compute a raster DTM with grid_terrain ---
#' # (or read it from a file)
#'
#' dtm = grid_terrain(las, method = "kriging", k = 10L)
#' nlas = lasnormalize(las, dtm)
#' plot(dtm)
#' plot(nlas)
#'
#' # --- Second option: interpolate each point (no discretization) ---
#'
#' nlas = lasnormalize(las, method = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
#' plot(nlas)
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
lasnormalize = function(.las, dtm = NULL, method = "none", k = 10L, model = gstat::vgm(.59, "Sph", 874))
{
  . <- Z <- Zn <- X <- Y <- Classification <- NULL

  stopifnotlas(.las)

  if(is.null(dtm))
  {
    normalized = LAS(data.table::copy(.las@data), .las@header)
    Zground = interpolate(.las@data[Classification == 2, .(X,Y,Z)], .las@data[, .(X,Y)], method = method, k = k, model = model)
    normalized@data[, Zn := Zground][]
    isna = is.na(Zground)
  }
  else
  {
    if(is(dtm, "lasmetrics"))
      dtm = as.raster(dtm)

    if(!is(dtm, "RasterLayer"))
      stop("The terrain model is not a RasterLayer or a lasmetrics", call. = F)

    normalized = LAS(data.table::copy(.las@data), .las@header)
    lasclassify(normalized, dtm, "Zn")
    isna = is.na(normalized@data$Zn)
  }

  if(sum(isna) > 0)
    warning(paste0(sum(isna), " points outside of the convex hull were removed."), call. = F)

  normalized@data[, Z := round(Z - Zn, 3)][, Zn := NULL][]
  normalized = lasfilter(normalized, !isna)

  return(normalized)
}

#' Convenient operator to lasnormalize
#'
#' @param e1 a LAS object
#' @param e2 a RasterLayer
#' @export
setMethod("-", c("LAS", "RasterLayer"), function(e1, e2)
{
  return(lasnormalize(e1,e2))
})
