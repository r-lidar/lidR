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
    driver <- drivers$LAS
  else if (inherits(x, "Raster"))
    driver <- drivers$Raster
  else if (inherits(x, "SpatialPoints") | inherits(x, "SpatialPolygons") | inherits(x, "SpatialLines"))
    driver <- drivers$Spatial
  else if (inherits(x, "sf"))
    driver <- drivers$SimpleFeature
  else if (inherits(x, "data.frame"))
    driver <- drivers$DataFrame
  else if (is(x, "lidr_internal_skip_write"))
    return(0)
  else
    stop(glue::glue("Trying to write an object of class {class(x)} but this type is not supported."))

  path <- paste0(path, driver$extension)
  driver$param[[driver$object]] <- x
  driver$param[[driver$path]]   <- path
  do.call(driver$write, driver$param)
  return(path)
}

# Function from raster and adapted to support multiple output format
writeSpatial = function(x, filename, overwrite = FALSE, ...)
{
  filename <- normalizePath(filename, winslash = "/", mustWork = FALSE)
  driver   <- guess_driver_can_write(filename)
  layer    <- tools::file_path_sans_ext(basename(filename))

  if (file.exists(filename) & !overwrite)
    stop('file exists, use overwrite=TRUE to overwrite it')

  if (!inherits(x, 'Spatial'))
    stop('To write a shapefile you need to provide an object of class Spatial*')

  if (inherits(x, 'SpatialGrid'))
    stop('These data cannot be written to a shapefile')

  if (inherits(x, 'SpatialPixels'))
  {
    if (.hasSlot(x, 'data'))
      x <- as(x, 'SpatialPointsDataFrame')
    else
      x <- as(x, 'SpatialPoints')

    warning('Writing SpatialPixels to a shapefile. Writing to a raster file format might be more desirable')
  }

  if (!.hasSlot(x, 'data'))
  {
    if (inherits(x, 'SpatialPolygons'))
      x <- sp::SpatialPolygonsDataFrame(x, data.frame(ID = 1:length(x)), match.ID = FALSE)
    else if (inherits(x, 'SpatialLines'))
      x <- sp::SpatialLinesDataFrame(   x, data.frame(ID = 1:length(x)), match.ID = FALSE)
    else if (inherits(x, 'SpatialPoints'))
      x <- sp::SpatialPointsDataFrame(  x, data.frame(ID = 1:length(x)), match.ID = FALSE)
    else
      stop('These data cannot be written to a shapefile')
  }

  rgdal::writeOGR(x, filename, layer, driver = driver, overwrite_layer = overwrite, ...)
}

# Function non exported from sf and build from sf source code because CRAN does not accept to use operator :::
guess_driver_can_write = function(dsn)
{
  assert_is_a_string(dsn)

  extension_map <- list(
    bna = "BNA", csv = "CSV", e00 = "AVCE00", gdb = "OpenFileGDB",
    geojson = "GeoJSON", gml = "GML", gmt = "GMT", gpkg = "GPKG",
    gps = "GPSBabel", gtm = "GPSTrackMaker", gxt = "Geoconcept",
    jml = "JML", kml = "KML", map = "WAsP", mdb = "Geomedia",
    nc = "netCDF", ods = "ODS", osm = "OSM", pbf = "OSM", shp = "ESRI Shapefile",
    sqlite = "SQLite", vdv = "VDV", xls = "xls", xlsx = "XLSX")

  prefix_map <- list(
    couchdb = "CouchDB", db2odbc = "DB2ODBC", dods = "DODS",
    gft = "GFT", mssql = "MSSQLSpatial", mysql = "MySQL", oci = "OCI",
    odbc = "ODBC", pg = "PostgreSQL", sde = "SDE")

  drivers <- sf::st_drivers()

  drv <- extension_map[tolower(tools::file_ext(dsn))]

  if (any(grep(":", gsub(":[/\\]", "/", dsn))))
    drv <- prefix_map[tolower(strsplit(dsn, ":")[[1]][1])]

  drv <- unlist(drv)

  if (is.null(drv))
    stop("Could not guess driver for ", dsn, call. = FALSE)

  if (is.na(drv))
    stop("Could not guess driver for ", dsn, call. = FALSE)

  if (is.na(match(drv, drivers$name)))
    stop(unlist(drv), " driver not available in supported drivers.'", call. = FALSE)

  if (!drivers[match(drv, drivers$name), "write"])
    stop("Driver ", drv, " cannot write.", call. = FALSE)

  return(drv)
}
