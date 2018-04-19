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
#' Subtract digital terrain model (DTM) from LiDAR data to create a dataset normalized with
#' the ground at 0. The DTM can originate from several sources e.g. from an external file or
#' computed by the user. It can also be computed on the fly. In this case the algorithm does
#' not use rasterized data and each point is interpolated. There is no inaccuracy due to the
#' discretization of the terrain and the resolution of the terrain is virtually infinite (but
#' it is slower).\cr
#' Depending on the interpolation method, the edges of the dataset can be more, or less poorly
#' interpolated. A buffer around the region of interest is always recommended to avoid edge
#' effects.
#'
#' \describe{
#' \item{\code{knnidw}}{Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse distance weighting (IDW). This is a fast but basic method for spatial
#' data interpolation.}
#' \item{\code{delaunay}}{Interpolation based on Delaunay triangulation. It performs a linear
#' interpolation within each triangle. There are usually a few points outside the convex hull,
#' determined by the ground points at the very edge of the dataset, which cannot be interpolated
#' with a triangulation. Extrapolation is done using knnidw.}
#' \item{\code{kriging}}{Interpolation is done by universal kriging using the \link[gstat:krige]{krige} function. This method combines the KNN approach with the kriging approach. For each point of #' interest it kriges the terrain using the k-nearest neighbour ground points. This method is more difficult to manipulate but it is also the most advanced method for interpolating spatial data. }
#' }
#'
#' \code{lasunnormalize} enables restoration of the original elevation in a memory efficient way
#' in the case when the original elevations are recorded in the columns \code{Zref} (i.e. if
#' the point cloud was normalized with the package lidR).
#'
#' @param las a LAS object
#' @param dtm a \link[raster:raster]{RasterLayer} or a \code{lasmetrics} object computed with
#' \link[lidR:grid_terrain]{grid_terrain}.
#' @param method character. Used if \code{dtm = NULL}. Can be \code{"knnidw"},
#' \code{"delaunay"} or \code{"kriging"} (see \link{grid_terrain} for more details).
#' @param k numeric. Used if \code{dtm = NULL}. Number of k-nearest neighbours when the selected
#' method is either \code{"knnidw"} or \code{"kriging"}.
#' @param p numeric. Power for inverse distance weighting. Default 2.
#' @param model Used if \code{dtm = NULL}. A variogram model computed with \link[gstat:vgm]{vgm}
#' when the selected method is \code{"kriging"}. If NULL it performs an ordinary or weighted least
#' squares prediction.
#' @param copy By default the point cloud is updated in place by reference. Users can force
#' the function to return a new point cloud. Set TRUE for compatibility with versions < 1.3.0
#' @return The function returns NULL. The LAS object is updated by reference. Z is now the
#' normalized elevation, A new column 'Zref' records the former elevation values. This is a
#' way to save memory by avoiding making copies of the point cloud. But if \code{copy = TRUE},
#' a new LAS object is returned and the original one is not modified.
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
#' lasnormalize(las, dtm)
#'
#' plot(dtm)
#' plot(las)
#'
#' # --- Second option: interpolate each point (no discretization) ---
#' las = readLAS(LASfile)
#'
#' lasnormalize(las, method = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
#' plot(las)
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
lasnormalize = function(las, dtm = NULL, method, k = 10L, p = 1, model = gstat::vgm(.59, "Sph", 874), copy = FALSE)
{
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  stopifnotlas(las)


  if(is.null(dtm))
  {
    if (! "Classification" %in% names(las@data))
      stop("No field 'Classification' found.", call. = FALSE)

    if (fast_countequal(las@data$Classification, 2) == 0)
      stop("No ground point found in the point cloud.", call. = FALSE)

    Zground = interpolate(las@data[Classification == 2, .(X,Y,Z)], las@data[, .(X,Y)], method = method, k = k, p = p, model = model)

    isna = is.na(Zground)
    nnas = sum(isna)

    if(nnas > 0)
      stop(paste0(nnas, " points were not normalizable. Process aborded."), call. = F)
  }
  else
  {
    if(is(dtm, "lasmetrics"))
      dtm = as.raster(dtm)

    if(!is(dtm, "RasterLayer"))
      stop("The terrain model is not a RasterLayer or a lasmetrics", call. = F)

    xres = raster::res(dtm)[1]
    xmin = dtm@extent@xmin
    ymin = dtm@extent@ymin
    dtm  = raster::as.matrix(dtm)
    Zground = fast_extract(dtm, las@data$X, las@data$Y, xmin, ymin, xres) # 15 times faster than raster::extract + much memory effcient

    isna = is.na(Zground)
    nnas = sum(isna)

    if(nnas > 0)
      stop(paste0(nnas, " points were not normalizable because the DTM contained NA values. Process aborded"), call. = F)
  }

  if (!copy)
  {
    las@data[, Zref := Z]
    las@data[, Z := round(Z - Zground, 3)]
    las@data[]
    update_list_by_ref(las@header@PHB, "Min Z", min(las@data$Z))
    update_list_by_ref(las@header@PHB, "Max Z", max(las@data$Z))
    lascheck(las@data, las@header)
    return(invisible())
  }
  else
  {
    norm = data.table::copy(las@data)
    norm[, Z := round(Z - Zground, 3)]
    return(LAS(norm, las@header))
  }
}

#' @rdname lasnormalize
#' @export
lasunnormalize = function(las)
{
  Z <- Zref <- NULL

  if (! "Zref" %in% names(las@data))
    stop("No field 'Zref' found.", call. = FALSE)

  las@data[, Z := Zref]
  las@data[, Zref := NULL]
  las@data[]

  return(invisible())
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
