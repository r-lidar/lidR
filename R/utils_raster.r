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


make_overlay_raster = function(las, res, start = c(0,0), subcircle = 0)
{
  if (is(res, "RasterLayer"))
  {
    resolution = raster::res(res)
    if (resolution[1] !=  resolution[2]) stop("Rasters with different x y resolutions are not supported")
    return(res)
  }

  bbox      <- raster::extent(las) + 2 * subcircle
  bbox@xmin <- round_any(bbox@xmin - 0.5 * res - start[1], res) + start[1]
  bbox@xmax <- round_any(bbox@xmax - 0.5 * res - start[1], res) + res + start[1]
  bbox@ymin <- round_any(bbox@ymin - 0.5 * res - start[2], res) + start[2]
  bbox@ymax <- round_any(bbox@ymax - 0.5 * res - start[2], res) + res + start[2]
  layout    <- suppressWarnings(raster::raster(bbox, res = res, crs = las@proj4string))
  return(layout)
}

merge_rasters = function(output)
{
  if (length(output) == 1)
    return(output)

  names    <- names(output[[1]])
  factor   <- output[[1]]@data@isfactor
  issingle <- sapply(output, function(x) { raster::nrow(x) == 1 & raster::ncol(x) == 1 })
  single   <- list()

  if (any(issingle))
  {
    single <- output[issingle]
    output <- output[!issingle]
  }

  output <- do.call(raster::merge, output)
  names(output) <- names

  if (is(output, "RasterBrick") & raster::inMemory(output))
    colnames(output@data@values) <- names

  for (pixel in single)
  {
    pix = raster::rasterToPoints(pixel, spatial = TRUE)
    output[pix] <- as.matrix(pix@data)
  }

  return(output)
}

build_vrt = function(output, vrt)
{
  if (!options("lidR.buildVRT")[[1]])
    return(unlist(output))

  if (!requireNamespace("gdalUtils", quietly = TRUE))
  {
    message("'gdalUtils' package is needed to build a virtual raster mosaic. Returns the list of written files instead.")
    return(unlist(output))
  }

  output <- unlist(output)
  folder <- dirname(output[1])
  file   <- paste0("/", vrt, ".vrt")
  vrt    <- paste0(folder, file)
  gdalUtils::gdalbuildvrt(output, vrt)
  output <- raster::stack(vrt)

  if (dim(output)[3] == 1)
    return(output[[1]])
  else
    return(output)
}

match_chm_and_seeds = function(chm, seeds, field)
{
  assert_is_all_of(chm, "RasterLayer")
  assert_is_all_of(seeds, "SpatialPointsDataFrame")
  stopif_forbidden_name(field)

  if(is.null(raster::intersect(raster::extent(chm), raster::extent(seeds))))
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
