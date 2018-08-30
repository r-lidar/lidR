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



#' Remove the topography from a point cloud
#'
#' Subtract digital terrain model (DTM) from LiDAR point cloud to create a dataset normalized with
#' the ground at 0. The DTM can originate from an external file or be computed by the user. It can
#' also be computed on the fly. In this case the algorithm does not use rasterized data and each point
#' is interpolated. There is no inaccuracy due to the discretization of the terrain and the resolution
#' of the terrain is virtually infinite (but it is slower).\cr
#' Depending on the interpolation method, the edges of the dataset can be more, or less poorly
#' interpolated. A buffer around the region of interest is always recommended to avoid edge
#' effects.
#'
#' \describe{
#' \item{\code{dtm}}{Normalization is done by substracting the digital terrain model to the point cloud.
#' The \code{RasterLayer} must encompass the whole point cloud. If a single point fall within a NA pixel
#' the methods will stop and throw an error.}
#' \item{\code{knnidw}}{Normalization is done by without rasterizationby interpolating each point.
#' Spatial interpolation is done using a k-nearest neighbour (KNN) approach with an inverse distance
#' weighting (IDW). This is a fast but basic method for spatial data interpolation.}
#' \item{\code{tin}}{Normalization is done by without rasterization by interpolating each point.
#' Spatial interpolation is based on a Delaunay triangulation. It performs a linear
#' interpolation within each triangle. There are usually a few points outside the convex hull,
#' determined by the ground points at the very edge of the dataset, which cannot be interpolated
#' with a triangulation. Extrapolation is done using knnidw.}
#' \item{\code{kriging}}{Normalization is done by without rasterizationby interpolating each point.
#' Spatial interpolation is done by universal kriging using the \link[gstat:krige]{krige} function.
#' This method combines the KNN approach with the kriging approach. For each point of interest it
#' kriges the terrain using the k-nearest neighbour ground points. This method is more difficult to
#' manipulate but it is also the most advanced method for interpolating spatial data. }
#' }
#'
#' \code{lasunnormalize} enables restoration of the original elevation in a memory efficient way
#' in the case when the original elevations are recorded in the columns \code{Zref} (i.e. if
#' the point cloud was normalized with the package lidR).
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param dtm a \link[raster:raster]{RasterLayer} representing a digital terrain model (can be
#' computed with \link{grid_terrain}).
#' @param method character. Can be \code{"dtm"}, \code{"knnidw"}, \code{"tin"} or \code{"kriging"}
#' (see details)
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' of the algorithms).
#' @param k numeric. Number of k-nearest neighbours. Default 10.
#' @param p numeric. Power for inverse distance weighting. Default 2.
#' @param model A variogram model computed with \link[gstat:vgm]{vgm}. If NULL it performs an ordinary
#' or weighted least squares prediction.
#'
#' @return If the input is a \code{LAS} object the function returns NULL. The LAS object is updated
#' by reference. Z is now the normalized elevation, A new column 'Zref' records the former elevation
#' values which enable to use \code{lasunormalize} to restore original point elevations.\cr
#' If the input is a \code{LAScatalog} object, a new \code{LAScatalog}.
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' plot(las)
#'
#' # First option: compute a raster DTM with grid_terrain (or read it from a file)
#' # ============================================================================
#'
#' dtm = grid_terrain(las, method = "kriging", k = 10L)
#' lasnormalize(las, method = "dtm", dtm = dtm)
#'
#' plot(dtm)
#' plot(las)
#'
#' # restore original elevations
#' lasunnormalize(las)
#' plot(las)
#'
#' # operator - can be used. This is equivalent to the previous line
#' las - dtm
#' plot(las)
#'
#' # restore original elevations
#' lasunnormalize(las)
#'
#' # Second option: interpolate each point (no discretization)
#' # =========================================================
#'
#' lasnormalize(las, method = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
#' plot(las)
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
lasnormalize = function(las, method, ...)
{
  if (method == "dtm" )
    return(lasnormalize_dtm(las, ...))
  else if (method == "knnidw")
    return(lasnormalize_knnidw(las, ...))
  else if (method == "tin")
    return(lasnormalize_tin(las, ...))
  else if (method == "kriging")
    return(lasnormalize_kriging(las, ...))
  else
    stop("This algorithm does not exist.", call. = FALSE)
}

#' @rdname lasnormalize
#' @export
lasnormalize_dtm = function(las, dtm)
{
  stopifnot(is(dtm, "RasterLayer"))
  lasnormalize_generic(las, dtm = dtm)
}

#' @rdname lasnormalize
#' @export
lasnormalize_tin = function(las)
{
  lasnormalize_generic(las, method = "delaunay")
}

#' @rdname lasnormalize
#' @export
lasnormalize_knnidw = function(las, k = 10L, p = 1)
{
  lasnormalize_generic(las, method = "knnidw", k = k, p = p)
}

#' @rdname lasnormalize
#' @export
lasnormalize_kriging = function(las, k = 10L, model = gstat::vgm(.59, "Sph", 874))
{
  lasnormalize_generic(las, method = "kriging", k = k, model = model)
}


lasnormalize_generic = function(las, dtm = NULL, method, k = 10L, p = 1, model = gstat::vgm(.59, "Sph", 874))
{
  UseMethod("lasnormalize_generic", las)
}

lasnormalize_generic.LAS = function(las, dtm = NULL, method, k = 10L, p = 1, model = gstat::vgm(.59, "Sph", 874))
{
  . <- Z <- Zref <- X <- Y <- Classification <- NULL

  # Point based normalization
  if(is.null(dtm))
  {
    if (! "Classification" %in% names(las@data))
      stop("No field 'Classification' found. This attribute is requiered to interpolate ground points.", call. = FALSE)

    if (fast_countequal(las@data$Classification, 2L) == 0)
      stop("No ground point found in the point cloud.", call. = FALSE)

    wbuffer = !"buffer" %in% names(las@data)
    Zground = interpolate(las@data[Classification == 2, .(X,Y,Z)], las@data[, .(X,Y)], method = method, k = k, p = p, model = model, wbuffer = wbuffer)

    isna = is.na(Zground)
    nnas = sum(isna)

    if(nnas > 0)
      stop(glue::glue("{nnas} points were not normalizable. Process aborded."), call. = FALSE)
  }
  # Raster based normalization
  else
  {
    #xres = raster::res(dtm)[1]
    #xmin = dtm@extent@xmin
    #ymin = dtm@extent@ymin
    #dtm  = raster::as.matrix(dtm)
    #Zground = fast_extract(dtm, las@data$X, las@data$Y, xmin, ymin, xres) # 15 times faster than raster::extract + much memory effcient
    Zground = raster::extract(dtm, las@data[, .(X,Y)])

    isna = is.na(Zground)
    nnas = sum(isna)

    if(nnas > 0)
      stop(glue::glue("{nnas} points were not normalizable because the DTM contained NA values. Process aborded."), call. = F)
  }

  if (!"Zref" %in% names(las@data))
    las@data[, Zref := Z]

  las@data[, Z := round(Z - Zground, 3)]
  lasupdateheader(las)
  return(invisible(las))
}

lasnormalize_generic.LAScluster = function(las, dtm = NULL, method, k = 10L, p = 1, model = gstat::vgm(.59, "Sph", 874))
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  lasnormalize_generic(x, dtm, method, k, p, model)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

lasnormalize_generic.LAScatalog = function(las, dtm = NULL, method, k = 10L, p = 1, model = gstat::vgm(.59, "Sph", 874))
{
  set_select(las) <- "*"

  output      <- catalog_apply2(las, lasnormalize_generic, dtm = dtm, method = method, k = k, p = p, model = model, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output      <- unlist(output)
  ctg         <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}

#' @rdname lasnormalize
#' @export
lasunnormalize = function(las)
{
  Z <- Zref <- NULL

  if ("Zref" %in% names(las@data))
  {
    las@data[, Z := Zref]
    las@data[, Zref := NULL]
    las@data[]
  }
  else
    message("No field 'Zref' found. Unormalizisation is impossible.")

  return(invisible())
}

#' Convenient operator to lasnormalize
#'
#' @param e1 a LAS object
#' @param e2 a RasterLayer
#' @export
#' @rdname lasnormalize
setMethod("-", c("LAS", "RasterLayer"), function(e1, e2)
{
  lasnormalize_dtm(e1,e2)
  return(invisible(las))
})
