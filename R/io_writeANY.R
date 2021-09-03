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

writeANY = function(x, path, drivers)
{
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (class(x)[1] %in% names(drivers))
    driver <- drivers[[class(x)[1]]]
  else if (inherits(x, "LAS"))
    driver <- drivers$LAS # nocov
  else if (inherits(x, "Raster"))
    driver <- drivers$Raster
  else if (inherits(x, "SpatialPoints") | inherits(x, "SpatialPolygons") | inherits(x, "SpatialLines"))
    driver <- drivers$Spatial
  else if (inherits(x, "sf"))
    driver <- drivers$SimpleFeature
  else if (inherits(x, "data.frame"))
    driver <- drivers$DataFrame
  else if (is(x, "lidr_internal_skip_write"))
    return(x)
  else
    stop(glue::glue("Trying to write an object of class {class(x)} but this type is not supported."))

  path <- paste0(path, driver$extension)
  driver$param[[driver$object]] <- x
  driver$param[[driver$path]]   <- path
  do.call(driver$write, driver$param)
  return(path)
}

writeSpatial = function(x, filename, overwrite = FALSE, ...)
{
  filename <- normalizePath(filename, winslash = "/", mustWork = FALSE)
  x <- sf::st_as_sf(x)

  if (isTRUE(overwrite))
    append = FALSE
  else
    append = NA

  sf::st_write(x, filename, append = append, quiet = TRUE)
}
