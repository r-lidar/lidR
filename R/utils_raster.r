# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2019 Jean-Romain Roussel
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

#' RasterLayer tools
#'
#' Internal function: a set of tools related to \code{Raster*}
#' \itemize{
#' \item \code{rOverlay}: Generates an (empty) \code{RasterLayer} that encompasse a point cloud
#' \item \code{rMergeList}: Merge a list of \code{Raster*} in a single \code{Raster*} preserving the
#' layer names
#' \item \code{rBuildVRT}: Create a Virtual Raster Mosaic from a set of Raster file on disk.
#' }
#'
#' @param las An object of class \code{LAS}
#' @param res numeric. resolution of the raster
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) meaning that
#' the grid aligns on (0,0).
#' @param buffer numeric. Extend the bounding box of the LAS object to generate a RasterLayer larger
#' than the point cloud
#' @param raster_list list. List of \code{Raster*} objects
#' @param file_list list. List of path to \code{Raster*} file
#' @param vrt character. Name of the VRT that must be written
#'
#' @keywords internal
#' @noRd
rOverlay = function(las, res, start = c(0,0), buffer = 0)
{
  if (is(res, "RasterLayer"))
  {
    resolution <- raster::res(res)
    if (round(resolution[1], 4) != round(resolution[2], 4))
      stop("Rasters with different x y resolutions are not supported", call. = FALSE)
    return(res)
  }

  bbox      <- raster::extent(las) + 2 * buffer
  bbox@xmin <- round_any(bbox@xmin - 0.5 * res - start[1], res) + start[1]
  bbox@xmax <- round_any(bbox@xmax - 0.5 * res - start[1], res) + res + start[1]
  bbox@ymin <- round_any(bbox@ymin - 0.5 * res - start[2], res) + start[2]
  bbox@ymax <- round_any(bbox@ymax - 0.5 * res - start[2], res) + res + start[2]
  layout    <- suppressWarnings(raster::raster(bbox, res = res, crs = las@proj4string))
  raster::projection(layout) <- raster::projection(las)
  return(layout)
}

#' @rdname rOverlay
rMergeList = function(raster_list)
{
  if (length(raster_list) == 1)
    return(raster_list)

  names    <- names(raster_list[[1]])
  factor   <- raster_list[[1]]@data@isfactor
  issingle <- sapply(raster_list, function(x) { raster::nrow(x) == 1 & raster::ncol(x) == 1 })
  single   <- list()

  if (any(issingle))
  {
    single <- raster_list[issingle]
    raster_list <- raster_list[!issingle]
  }

  raster <- do.call(raster::merge, raster_list)
  names(raster) <- names

  if (is(raster, "RasterBrick") && raster::inMemory(raster))
    colnames(raster@data@values) <- names

  for (pixel in single)
  {
    pix = raster::rasterToPoints(pixel, spatial = TRUE)
    raster[pix] <- as.matrix(pix@data)
  }

  return(raster)
}

#' @rdname rOverlay
rBuildVRT = function(file_list, vrt)
{
  if (!options("lidR.buildVRT")[[1]])
    return(unlist(file_list))

  if (!requireNamespace("gdalUtils", quietly = TRUE))
  {
    message("'gdalUtils' package is needed to build a virtual raster mosaic. Returns the list of written files instead.")
    return(unlist(file_list))
  }

  file_list <- unlist(file_list)
  folder    <- dirname(file_list[1])
  file      <- paste0("/", vrt, ".vrt")
  vrt       <- paste0(folder, file)
  gdalUtils::gdalbuildvrt(file_list, vrt, vrtnodata = -Inf)
  file_list <- raster::stack(vrt)

  if (dim(file_list)[3] == 1)
    return(file_list[[1]])
  else
    return(file_list)
}

match_chm_and_seeds = function(chm, seeds, field)
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_all_of(seeds, "SpatialPointsDataFrame")
  stopif_forbidden_name(field)

  if (is.null(raster::intersect(raster::extent(chm), raster::extent(seeds))))
    stop("No seed matches with the canopy height model" )

  if (field %in% names(seeds@data))
  {
    ids = seeds@data[[field]]

    if (!is.numeric(ids))
      stop("Tree IDs much be of numeric type.",  call. = FALSE)

    if (length(unique(ids)) < length(ids))
      stop("Duplicated tree IDs found.")
  }
  else
    ids = 1:nrow(seeds@data)

  cells = raster::cellFromXY(chm, seeds)

  if (anyNA(cells))
  {
    if (all(is.na(cells)))
      stop("No seed found")
    else
      warning("Some seeds are outside the canopy height model. They were removed.")

    no_na = !is.na(cells)
    seeds = seeds[no_na,]
    cells = cells[no_na]
  }

  return(list(cells = cells, ids = ids))
}
