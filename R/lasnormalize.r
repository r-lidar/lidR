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
#' several sources e.g. from an external file or computed by the user. It can also be computed
#' on the fly. In this case each point is interpolated and there is no innacuracy due
#' to the discretization of the terrain.
#'
#' @param .las a LAS objet
#' @param dtm a \link[raster:raster]{RasterLayer} or a \code{lasmetrics} object.
#' \link[lidR:grid_terrain]{grid_terrain}.
#' @param ... if \code{dtm = NULL} the algorithm does not use rasterized data. It computes
#' the interpolation for each point (much slower). User can pass a method of interpolation
#' and the parameters requiered for this method. Avaible methods and parameters are the
#' same as in \link{grid_terrain}. Refer to this function.
#' @return A LAS object.
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' plot(las)
#'
#' # --- First option: compute a raster DTM with grid_terrain -----
#' # (or read it from a file)
#'
#' dtm = grid_terrain(las, method = "kriging", k = 8L)
#' nlas = lasnormalize(las, dtm)
#' plot(dtm)
#' plot(nlas)
#'
#' # --- Second option: interpol each point (no discretization) -----
#'
#' nlas = lasnormalize(las, method = "kriging", k = 8L, model = gstat::vgm(0.59, "Sph", 874))
#' plot(nlas)
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
lasnormalize = function(.las, dtm = NULL, ...)
{
  . <- Z <- Zn <- X <- Y <- Classification <- NULL

  stopifnotlas(.las)

  if(is.null(dtm))
  {
    dots = list(...)

    if(dots$method == "delaunay")
      stop("Method 'delaunay' not avaible for non rasterized normalization.", call. = F)

    normalized = LAS(data.table::copy(.las@data), .las@header)
    Zground = interpolate(.las@data[Classification == 2, .(X,Y,Z)], .las@data[, .(X,Y)], ...)
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
    warning(paste0(sum(isna), " points with NA elevation points found and removed."), call. = F)

  normalized@data[, Z := round(Z - Zn, 3)][, Zn := NULL][]
  normalized = lasfilter(normalized, !isna)

  return(normalized)
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
