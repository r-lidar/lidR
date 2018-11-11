#' @return A \code{RasterLayer} or a \code{RasterBrick} containing a numeric value in each cell. If the
#' \code{RasterLayer}s are written on disk when running the function with a \code{LAScatalog}, a
#' virtual raster mosaic is returned (see \link[gdalUtils:gdalbuildvrt]{gdalbuildvrt})
