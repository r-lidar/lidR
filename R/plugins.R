LIDRDSM <- "DigitalSurfaceModel"
LIDRSPI <- "SpatialInterpolation"
LIDRGND <- "GroundSegmentation"
LIDRITD <- "IndividualTreeDetection"
LIDRDEC <- "PointCloudDecimation"
LIDRSHP <- "ShapeDetection"
LIDRSNG <- "SnagsSegmentation"
LIDRITS <- "IndividualTreeSegmentation"
LIDRNIT <- "normalize_intensity"
LIDRTRK <- "SensorTracking"
LIDROUT <- "NoiseSegmentation"

LIDRCONTEXTDSM <- "rasterize_canopy"
LIDRCONTEXTSPI <- c("normalize_height", "rasterize_terrain", "p2r", "spatial_interpolation")
LIDRCONTEXTGND <- "classify_ground"
LIDRCONTEXTOUT <- "classify_noise"
LIDRCONTEXTITD <- "locate_trees"
LIDRCONTEXTDEC <- "decimate_points"
LIDRCONTEXTSHP <- "segment_shapes"
LIDRCONTEXTSNG <- "segment_snags"
LIDRCONTEXTITS <- "segment_trees"
LIDRCONTEXTNIT <- "normalize_intensity"
LIDRCONTEXTTRK <- "track_sensor"

LIDRALGORITHM <- "lidRAlgorithm"
LIDRALGORITHMGENERIC = c(LIDRALGORITHM, "ANY", "function")
LIDRALGORITHMOPENMP <- "omp"
LIDRALGORITHMPOINTCLOUDBASED <- "PointCloudBased"
LIDRALGORITHMRASTERBASED <- "RasterBased"
LIDRALGORITHMDEC <- LIDRALGORITHMDSM <- LIDRALGORITHMGND <- LIDRALGORITHMITD <- LIDRALGORITHMITS <- LIDRALGORITHMNIT <- LIDRALGORITHMSNG <- LIDRALGORITHMTRK <- LIDRALGORITHMSHP <- LIDRALGORITHMSPI <- LIDRALGORITHMOUT <- LIDRALGORITHMGENERIC
LIDRALGORITHMDEC[2] <- LIDRDEC
LIDRALGORITHMDSM[2] <- LIDRDSM
LIDRALGORITHMGND[2] <- LIDRGND
LIDRALGORITHMITD[2] <- LIDRITD
LIDRALGORITHMITS[2] <- LIDRITS
LIDRALGORITHMNIT[2] <- LIDRNIT
LIDRALGORITHMSNG[2] <- LIDRSNG
LIDRALGORITHMTRK[2] <- LIDRTRK
LIDRALGORITHMSHP[2] <- LIDRSHP
LIDRALGORITHMSPI[2] <- LIDRSPI
LIDRALGORITHMOUT[2] <- LIDROUT

plugin <- function(f, class_t, omp = FALSE)
{
  if (omp) class_t <- c(class_t, LIDRALGORITHMOPENMP)
  class(f) <- class_t
  return(f)
}


#' Plugin system
#'
#' Tools to build plugin functions for lidR
#'
#' @param f a function
#' @param omp logical is the function natively parallized with OpenMP
#' @param raster_based logical. For ITS and ITD algorithms, is the method raster-based or
#' or point-cloud-based?
#'
#' @name plugins
#' @rdname plugins
#' @examples
#' \dontrun{
#' mba <- function(n = 1, m = 1, h = 8, extend = TRUE) {
#'   f <- function(las, where) {
#'     res <- MBA::mba.points(las@data, where, n, m , h, extend)
#'     return(res$xyz.est[,3])
#'   }
#'
#'   f <- plugin_dtm(f)
#'   return(f)
#' }
#'
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' dtm = rasterize_terrain(las, algorithm = mba())
#' }
NULL

#' @export
#' @rdname plugins
plugin_dsm      <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMDSM, omp))

#' @export
#' @rdname plugins
plugin_dtm      <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMSPI, omp))

#' @export
#' @rdname plugins
plugin_gnd      <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMGND, omp))

#' @export
#' @rdname plugins
plugin_decimate <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMDEC, omp))

#' @export
#' @rdname plugins
plugin_shape    <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMSHP, omp))

#' @export
#' @rdname plugins
plugin_snag     <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMSNG, omp))

#' @export
#' @rdname plugins
plugin_track    <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMTRK, omp))

#' @export
#' @rdname plugins
plugin_nintensity    <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMNIT, omp))

#' @export
#' @rdname plugins
plugin_outliers    <- function(f, omp = FALSE) return(plugin(f, LIDRALGORITHMOUT, omp))

#' @export
#' @rdname plugins
plugin_itd    <- function(f, omp = FALSE, raster_based = FALSE)
{
  if (isTRUE(raster_based))
    return(plugin(f, c(LIDRALGORITHMITD, LIDRALGORITHMRASTERBASED), omp))
  else
    return(plugin(f, c(LIDRALGORITHMITD, LIDRALGORITHMPOINTCLOUDBASED), omp))
}

#' @export
#' @rdname plugins
plugin_its    <- function(f, omp = FALSE, raster_based = FALSE)
{
  if (isTRUE(raster_based))
    return(plugin(f, c(LIDRALGORITHMITS, LIDRALGORITHMRASTERBASED), omp))
  else
    return(plugin(f, c(LIDRALGORITHMITS, LIDRALGORITHMPOINTCLOUDBASED), omp))
}
