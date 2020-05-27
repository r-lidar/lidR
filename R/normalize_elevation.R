# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
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
#' the ground at 0. The DTM can originate from an external file or can be computed by the user. It can
#' also be computed on-the-fly. In this case the algorithm does not use rasterized data and each point
#' is interpolated. There is no inaccuracy due to the discretization of the terrain and the resolution
#' of the terrain is virtually infinite.\cr\cr
#' How well the edges of the dataset are interpolated depends on the interpolation method used.
#' Thus, a buffer around the region of interest is always recommended to avoid edge effects.\cr\cr
#' The attribute Z of the returned LAS object is the normalized elevation. A new attribute 'Zref'
#' records the former elevation values, which enables the use of \link{unnormalize_height} to restore
#' original point elevations.\cr\cr
#'
#' @template param-las
#'
#' @param algorithm a spatial interpolation function. \code{lidR} have \link{tin},
#' \link{kriging}, \link{knnidw} or a \link[raster:raster]{RasterLayer} representing a digital terrain
#' model (can be computed with \link{grid_terrain})
#' @param na.rm logical. When using a \code{RasterLayer} as DTM, by default the function fails if a point
#' fall in an empty pixel because a Z elevation cannot be NA. If \code{na.rm = TRUE} points with an
#' elevation of NA are filtered. Be careful this creates a copy of the point cloud.
#' @param use_class integer vector. By default the terrain is computed by using ground points
#' (class 2) and water points (class 9). Relevant only for a normalisation without a raster DTM.
#' @param ... If \code{algorithm} is a \code{RasterLayer}, \code{...} is propagated to
#' \link[raster:extract]{extract}. Typically one may use \code{method = "bilinear"}.
#' @param add_lasattribute logical. By default the above see level elevation is retained in a new attribute.
#' However this new attribute will be discared at write time. If \code{TRUE} it is maintained as an
#' extrabytes attribute. See also \link{add_lasattribute}.
#' @param Wdegenerated logical. The function always check and remove degenerated ground points. If
#' any a warning in thrown.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' plot(las)
#'
#' # First option: use a RasterLayer as DTM
#' # =======================================================
#'
#' dtm <- grid_terrain(las, 1, knnidw(k = 6L, p = 2))
#' las <- normalize_height(las, dtm)
#'
#' plot(dtm)
#' plot(las)
#'
#' # restore original elevations
#' las <- unnormalize_height(las)
#' plot(las)
#'
#' # operator - can be used. This is equivalent to the previous
#' las <- las - dtm
#' plot(las)
#'
#' # restore original elevations
#' las <- unnormalize_height(las)
#'
#' # Second option: interpolate each point (no discretization)
#' # =========================================================
#'
#' las <- normalize_height(las, tin())
#' plot(las)
#'
#' # operator - can be used. This is equivalent to the previous
#' las <- unnormalize_height(las)
#' las <- las - tin()
#'
#' \dontrun{
#' # All the following syntaxes are correct
#' las <- normalize_height(las, knnidw())
#' las <- normalize_height(las, knnidw(k = 8, p = 2))
#' las <- las - knnidw()
#' las <- las - knnidw(k = 8)
#' las <- normalize_height(las, kriging())
#' las <- las - kriging(k = 8)
#' }
#'
#' @seealso
#' \link[lidR:grid_terrain]{grid_terrain}
#' @family normalize
#' @export
#' @rdname normalize_height
normalize_height = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  UseMethod("normalize_height", las)
}

#' @export
normalize_height.LAS = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  assert_is_a_bool(na.rm)
  assert_is_a_bool(add_lasattribute)
  assert_is_a_bool(Wdegenerated)

  if (is(algorithm, "RasterLayer"))
  {
    Zground <- raster::extract(algorithm, coordinates(las), ...)
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if (nnas > 0 && na.rm == FALSE)
      stop(glue::glue("{nnas} points were not normalizable because the DTM contained NA values. Process aborted."))
  }
  else if (is.function(algorithm))
  {
    assert_is_algorithm(algorithm)
    assert_is_algorithm_spi(algorithm)

    if (any(as.integer(use_class) != use_class))
      stop("'add_class' is not a vector of integers'", call. = FALSE)

    use_class <- as.integer(use_class)

    if (!"Classification" %in% names(las@data))  stop("No field 'Classification' found. This attribute is required to interpolate ground points.")
    if (fast_countequal(las@data$Classification, LASGROUND) == 0) stop("No ground point found in the point cloud.")

    # Non standart evaluation (R CMD check)
    . <- Z <- Zref <- X <- Y <- Classification <- NULL

    # Delaunay triangulation with boost requiere to
    # compute back integer coordinates
    xscale  <- las@header@PHB[["X scale factor"]]
    yscale  <- las@header@PHB[["Y scale factor"]]
    xoffset <- las@header@PHB[["X offset"]]
    yoffset <- las@header@PHB[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)

    # Select the ground points
    ground  <- las@data[Classification %in% c(use_class), .(X,Y,Z)]
    ground  <- check_degenerated_points(ground, Wdegenerated)

    # wbuffer = !"buffer" %in% names(las@data)
    lidR.context <- "normalize_height"
    Zground <- algorithm(ground, las@data, scales, offsets)
    isna    <- is.na(Zground)
    nnas    <- sum(isna)

    if (nnas > 0 & na.rm == FALSE)
      stop(glue::glue("{nnas} points were not normalizable. Process aborted."))
  }
  else
  {
    stop(glue::glue("Parameter 'algorithm' is a {class(algorithm)}. Expected type is 'RasterLayer' or 'function'"), call. = FALSE)
  }

  zoffset <- las@header@PHB[["Z offset"]]
  zscale <- las@header@PHB[["Z scale factor"]]

  if (!"Zref" %in% names(las@data))
    las@data[["Zref"]] <- las@data[["Z"]]

  las@data[["Z"]] <- las@data[["Z"]] - Zground

  if (add_lasattribute && is.null(las@header@VLR$Extra_Bytes[["Extra Bytes Description"]][["Zref"]]))
    las <- add_lasattribute_manual(las, name = "Zref", desc = "Elevation above sea level", type = "int", offset = zoffset, scale = zscale)

  if (nnas > 0 && na.rm == TRUE)
  {
    las <- filter_poi(las, !isna)
    message(glue::glue("{nnas} points were not normalizable and removed."))
  }

  fast_quantization(las@data[["Z"]], zscale, zoffset)
  las <- lasupdateheader(las)
  return(las)
}

#' @export
normalize_height.LAScluster = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- normalize_height(x, algorithm, na.rm, use_class, ..., add_lasattribute = add_lasattribute, Wdegenerated = Wdegenerated)
  x <- filter_poi(x, buffer == 0)
  return(x)
}

#' @export
normalize_height.LAScatalog = function(las, algorithm, na.rm = FALSE, use_class = c(2L,9L), ..., add_lasattribute = FALSE, Wdegenerated = TRUE)
{
  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, normalize_height, algorithm = algorithm, na.rm = na.rm, use_class = use_class, ..., add_lasattribute = add_lasattribute, Wdegenerated = Wdegenerated, .options = options)
  return(output)
}

#' @rdname normalize_height
#' @export
unnormalize_height = function(las)
{
  stopifnotlas(las)

  if ("Zref" %in% names(las@data))
  {
    las@data[["Z"]] <- las@data[["Zref"]]
    las@data[["Zref"]] <- NULL
    las <- lasupdateheader(las)
  }
  else
    message("No attribute 'Zref' found. Un-normalizisation is impossible.")

  return(las)
}

#' @param e1 a LAS object
#' @param e2 \link[raster:raster]{RasterLayer} representing a digital terrain model (can be
#' computed with \link{grid_terrain}) or a spatial interpolation function. \code{lidR} has \link{tin},
#' \link{kriging}, and \link{knnidw}.
#' @export
#' @rdname normalize_height
setMethod("-", c("LAS", "RasterLayer"), function(e1, e2)
{
  return(normalize_height(e1,e2))
})

setOldClass("lidRAlgorithm")

#' @export
#' @rdname normalize_height
  setMethod("-", c("LAS", "lidRAlgorithm"), function(e1, e2)
{
  return(normalize_height(e1,e2))
})

check_degenerated_points = function(points, Wdegenerated = TRUE)
{
  . <- X <- Y <- Z <- NULL

  # test integrity of the data and degenerated points
  dup_xyz  = duplicated(points, by = c("X", "Y", "Z"))
  dup_xy   = duplicated(points, by = c("X", "Y"))
  ndup_xyz = sum(dup_xyz)
  ndup_xy  = sum(dup_xy & !dup_xyz)

  if (ndup_xyz > 0 && Wdegenerated)
    warning(glue::glue("There were {ndup_xyz} degenerated ground points. Some X Y Z coordinates were repeated. They were removed."), call. = FALSE)

  if (ndup_xy > 0 && Wdegenerated)
    warning(glue::glue("There were {ndup_xy} degenerated ground points. Some X Y coordinates were repeated but with different Z coordinates. min Z were retained."), call. = FALSE)

  if (ndup_xy > 0 | ndup_xyz > 0)
    points = points[, .(Z = min(Z)), by = .(X,Y)]

  return(points)
}
