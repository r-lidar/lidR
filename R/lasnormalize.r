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
#' of the terrain is virtually infinite.\cr\cr
#' Depending on the interpolation method, the edges of the dataset can be more, or less poorly
#' interpolated. A buffer around the region of interest is always recommended to avoid edge
#' effects.\cr\cr
#' \code{lasunnormalize} enables restoration of the original elevation in a memory efficient way
#' in the case when the original elevations are recorded in the columns \code{Zref} (i.e. if
#' the point cloud was normalized with the package lidR).
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param algorithm a \link[raster:raster]{RasterLayer} representing a digital terrain model (can be
#' computed with \link{grid_terrain}) or a \link[lidR:SpatialInterpolationFunctions]{Spatial Interpolation Function}
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
#' # First option: use a RasterLayer as DTM
#' # =======================================================
#'
#' dtm = grid_terrain(las, method = "kriging", k = 10L)
#' lasnormalize(las, dtm)
#'
#' plot(dtm)
#' plot(las)
#'
#' # restore original elevations
#' lasunnormalize(las)
#' plot(las)
#'
#' # operator - can be used. This, is equivalent to the previous
#' las - dtm
#' plot(las)
#'
#' # restore original elevations
#' lasunnormalize(las)
#'
#' # Second option: interpolate each point (no discretization)
#' # =========================================================
#'
#' lasnormalize(las, tin())
#' plot(las)
#'
#' # operator - can be used. This, is equivalent to the previous
#' lasunnormalize(las)
#' las - tin()
#'
#' \dontrun{
#' # All the following syntaxes are correct
#' lasnormalize(las, knnidw())
#' lasnormalize(las, knnidw(k = 8, p = 2))
#' las - knnidw()
#' las - knnidw(k = 8)
#' lasnormalize(las, kriging())
#' las - kriging(k = 8)
#' }
#'
#' @seealso
#' \link[raster:raster]{raster}
#' \link[lidR:grid_terrain]{grid_terrain}
#' @export
#' @rdname lasnormalize
#' @export
lasnormalize = function(las, algorithm)
{
  UseMethod("lasnormalize", las)
}

#' @export
lasnormalize.LAS = function(las, algorithm)
{
  if (is(algorithm, "RasterLayer"))
  {
    Zground <- raster::extract(algorithm, las@data[, .(X,Y)])
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if(nnas > 0)
      stop(glue::glue("{nnas} points were not normalizable because the DTM contained NA values. Process aborded."), call. = F)
  }
  else if (is.function(algorithm))
  {
    if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
      stop("Invalid function provided as algorithm.", call. = FALSE)

    if (!is(algorithm, "SpatialInterpolation"))
      stop("The algorithm is not an algorithm for spatial interpolation.", call. = FALSE)

    . <- Z <- Zref <- X <- Y <- Classification <- NULL

    if (! "Classification" %in% names(las@data))
      stop("No field 'Classification' found. This attribute is requiered to interpolate ground points.", call. = FALSE)

    if (fast_countequal(las@data$Classification, LASGROUND) == 0)
      stop("No ground point found in the point cloud.", call. = FALSE)

    # wbuffer = !"buffer" %in% names(las@data)
    lidR.context <- "lasnormalize"
    ground  <- las@data[Classification == 2, .(X,Y,Z)]
    ground  <- check_degenerated_points(ground)
    Zground <- algorithm(ground, las@data)
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if(nnas > 0)
      stop(glue::glue("{nnas} points were not normalizable. Process aborded."), call. = FALSE)
  }
  else
  {
    stop(glue::glue("Parameter 'algorithm' is a {class(algorithm)}. Expected type is 'RasterLayer' or 'function'"), call. = FALSE)
  }

  if (!"Zref" %in% names(las@data))
    las@data[, Zref := Z]

  las@data[, Z := round(Z - Zground, 3)]
  lasupdateheader(las)
  return(invisible(las))
}

#' @export
lasnormalize.LAScluster = function(las, algorithm)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  lasnormalize(x, algorithm)
  x <- lasfilter(x, buffer == 0)
  return(x)
}

#' @export
lasnormalize.LAScatalog = function(las, algorithm)
{
  set_select(las) <- "*"

  output      <- catalog_apply2(las, lasnormalize, algorithm = algorithm, need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)
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

  return(invisible(las))
}

#' @param e1 a LAS object
#' @param e2 a \link[raster:raster]{RasterLayer} representing a digital terrain model (can be
#' computed with \link{grid_terrain}) or a \link[lidR:SpatialInterpolationFunctions]{Spatial Interpolation Function}
#' @export
#' @rdname lasnormalize
setMethod("-", c("LAS", "RasterLayer"), function(e1, e2)
{
  lasnormalize(e1,e2)
  return(invisible())
})

#' @export
#' @rdname lasnormalize
setMethod("-", c("LAS", "function"), function(e1, e2)
{
  lasnormalize(e1,e2)
  return(invisible())
})

check_degenerated_points = function(points)
{
  . <- X <- Y <- Z <- NULL

  # test integrity of the data and degenerated points
  dup_xyz  = duplicated(points, by = c("X", "Y", "Z"))
  dup_xy   = duplicated(points, by = c("X", "Y"))
  ndup_xyz = sum(dup_xyz)
  ndup_xy  = sum(dup_xy)

  if (ndup_xyz > 0)
    warning(glue::glue("There were {ndup_xyz} degenerated ground points. Some X Y Z coordinates were repeated. They were removed."), call. = FALSE)

  if (ndup_xy > 0)
    warning(glue::glue("There were {ndup_xy} degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates. min Z were retained."), call. = FALSE)

  if (ndup_xy > 0 | ndup_xyz > 0)
    points = points[, .(Z = min(Z)), by = .(X,Y)]

  return(points)
}
