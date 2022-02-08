#' LAScatalog drivers
#'
#' This document explains how objects are written on disk when processing a LAScatalog. As mentioned
#' in \link{LAScatalog-class}, users can set a templated filename to store the outputs on disk instead
#' of in R memory. By defaut `LAS` objects are stored in .las files with \link{writeLAS},
#' raster objects are stored in .tif files using native write function for `raster`, `stars` or `terra`,
#' `Spatial*` objects are stored in .shp files with after coercion to `sf` with \link[sf:st_write]{st_write},
#' `sf` object are stored to `.gpkg` files with \link[sf:st_write]{st_write}.`data.frame` objects are
#' stored in .csv files with \link[data.table:fwrite]{fwrite}, and other objects are not supported.
#' However, users can modify all these default settings and even add new drivers.
#' This manual page explain how. One may also refer to some unofficial documentation
#' \href{https://github.com/r-lidar/lidR/wiki/Modify-the-LAScatalog-drivers}{here} or
#' \href{https://gis.stackexchange.com/questions/325367/how-to-configure-lidr-catalog-to-save-raster-files}{here}.
#'
#' @section Generic form of a driver:
#' A driver is stored in the  slot `@output_options` of a `LAScatalog`. It is a list that contains:
#' \describe{
#' \item{write}{A function that receives an object and a path, and writes the object into a file using
#' the path. The function can also have extra options.}
#' \item{extension}{A string that gives the file extension.}
#' \item{object}{A string that gives the name of the argument used to pass the object to write in the
#' function used to write the object.}
#' \item{path}{A string that gives the name of the argument used to pass the path of the file to write
#' in the function used to write the object.}
#' \item{param}{A labelled list of extra parameters for the function used to write the object}
#' }
#' For example, the driver to write a  `Raster*` is
#' \preformatted{
#' list(
#'  write = raster::writeRaster,
#'  extension = ".tif",
#'  object = "x",
#'  path = "filename",
#'  param = list(format = "GTiff"))
#' }
#' And the driver to write a `LAS` is
#' \preformatted{
#' list(
#'  write = lidR::writeLAS,
#'  extension = ".las",
#'  object = "las",
#'  path = "file",
#'  param = list())
#' }
#'
#' @section Modify a driver (1/2):
#' Users can modify the drivers to write different file types than the default. For example, to write in
#' shapefile instead of a GeoPackage, one must change the `sf` driver:
#' \preformatted{
#' ctg@output_options$drivers$sf$extension <- ".shp"
#' }
#' To write a `Raster*` in .grd files instead of .tif files one must change the `Raster` driver:
#' \preformatted{
#' ctg@output_options$drivers$Raster$extension <- ".grd"
#' ctg@output_options$drivers$Raster$param$format <- "raster"
#' }
#' To write in .laz files instead of .las files one must change the `LAS` driver:
#' \preformatted{
#' ctg@output_options$drivers$LAS$extension <- ".laz"
#' }
#' @section Add a new driver:
#' The drivers allow `LAS`, `Spatial`,`sf,` `Raster`, `stars`, `SpatRaster` and `data.frame` objects
#' to be written. When using the engine (\link{catalog_apply}) to build new tools, users may need to
#' be able to write other objects such as a `list`. To do that users need to add a `list` element
#' into `@output_options`:
#' \preformatted{
#' ctg@output_options$drivers$list = list(
#'  write = base::saveRDS,
#'  object = "object",
#'  path = "file",
#'  extension = ".rds",
#'  param = list(compress = TRUE))
#' }
#' The `LAScatalog` now has a new driver capable of writing a `list`.
#'
#' @section Modify a driver (2/2):
#' It is also possible to completely overwrite an existing driver. By default `sf` objects are written
#' into GeoPackage with \link[sf:st_write]{st_write}. `st_write` can also wite in GeoJSON and even in
#' SQLlite database objects. But it cannot add data into an existing SQLlite database. Let's create
#' our own driver for a `sf`. First we need a function able to write and append a `sf` into a `SQLlite`
#' database from the object and the path.
#' \preformatted{
#' dbWrite_sf = function(x, path, name)
#' {
#'  x <- sf::st_drop_geometry(x)
#'  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
#'  RSQLite::dbWriteTable(con, name, x, append = TRUE)
#'  RSQLite::dbDisconnect(con)
#' }}
#' Then we create the driver. User-defined drivers supersede default drivers:
#' \preformatted{
#' ctg@output_options$drivers$sf = list(
#'  write = dbWrite_sf,
#'  extension = ".sqlite",
#'  object = "x",
#'  path = "path",
#'  param = list(name = "layername"))
#' }
#' Then to be sure that we do not write several .sqlite files, we don't use templated filename.
#' \preformatted{
#' opt_output_files(ctg) <- paste0(tempdir(), "/mysqlitefile")}
#' And all the `sf` will be appended in a single database. To preserve the geometry one can
#' @name lidR-LAScatalog-drivers
#' @md
NULL
