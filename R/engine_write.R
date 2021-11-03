#' @rdname engine
#' @export
#' @param path strings
#' @param drivers list. Drivers of a LAScatalog
engine_write = function(x, path, drivers)
{
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (class(x)[1] %in% names(drivers))
    driver <- drivers[[class(x)[1]]]
  else if (inherits(x, "LAS"))
    driver <- drivers$LAS # nocov
  else if (inherits(x, "stars"))
    driver <- drivers$stars
  else if (inherits(x, "Raster"))
    driver <- drivers$Raster
  else if (inherits(x, "Spatial"))
    driver <- drivers$Spatial
  else if (inherits(x, "sf"))
    driver <- drivers$sf
  else if (inherits(x, "data.frame"))
    driver <- drivers$data.frame
  else if (is(x, "lidr_internal_skip_write"))
    return(x)
  else
    stop(glue::glue("Trying to write an object of class {class(x)} but this type is not supported."))

  path <- paste0(path, driver$extension)
  if (is_raster(x)) {
    attr(path, "rasterpkg") <- raster_pkg(x)
    attr(path, "layernames") <- raster_names(x)
  }

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
