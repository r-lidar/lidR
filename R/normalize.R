#' Normalize point cloud
#'
#' Normalize elevation or intensity values using multiple methods.
#'
#' \describe{
#' \item{normalize_height}{Subtract digital terrain model (DTM) from a LiDAR point cloud to create a
#' dataset normalized with the ground at 0. The DTM can be a raster, but it can also be computed
#' on-the-fly. In this case the algorithm does not use rasterized data and each point is interpolated.
#' There is no inaccuracy due to the discretization of the terrain and the resolution of the terrain
#' is virtually infinite. A new attribute 'Zref' records the former elevation values, which enables
#' the use of \link{unnormalize_height} to restore original point elevations.}
#' \item{normalize_intensity}{Normalize intensity values using multiple methods. The attribute 'Intensity'
#' records the normalized intensity. An extra attribute named 'RawIntensity' records the original
#' intensities.}
#' }
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file format
#' and all the attributes. `select = "*"` is imposed internally.
#'
#' @template param-las
#' @param algorithm (1) An algorithm for spatial interpolation. \code{lidR} has \link{tin},
#' \link{kriging}, \link{knnidw} or a raster representing a digital terrain
#' model. (2) An algorithm for intensity normalization. \code{lidR} currently has \link{range_correction}.
#' @param use_class integer vector. By default the terrain is computed by using ground points
#' (class 2) and water points (class 9). Relevant only for a normalization without a raster DTM.
#' @param ... `normalized_height()` supports `add_lasattribute= TRUE` to add the elevation above
#' see level as an extra byte attribute and ` Wdegenerated = FALSE` to silence the warning about
#' degenerated ground points.
#' @param dtm raster. If `dtm` is provided, then the DTM is used in place of ground points. This is
#' different than providing a DTM in `algorithm`. If `algorithm = dtm` the dtm is subtracted naively.
#' If `algorithm = tin()` and `dtm = raster` the ground points are not used and the DTM is
#' interpolated as if it were made of regularly-spaced ground points.
#'
#' @name normalize
#' @rdname normalize
#' @md
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile)
#'
#' # ====================
#' # Normalize elevation
#' # ====================
#'
#' # First option: use a raster as DTM
#' # --------------------------------------
#'
#' dtm <- rasterize_terrain(las, 1, knnidw(k = 6L, p = 2))
#' nlas <- normalize_height(las, dtm)
#'
#' # restore original elevations
#' las <- unnormalize_height(nlas)
#'
#' # operator - can be used. This is equivalent to the previous
#' nlas <- las - dtm
#'
#' # restore original elevations
#' las <- unnormalize_height(las)
#'
#' # Second option: interpolate each point (no discretization)
#' # ---------------------------------------------------------
#'
#' nlas <- normalize_height(las, tin())
#'
#' # operator - can be used. This is equivalent to the previous
#' las <- unnormalize_height(nlas)
#' nlas <- las - tin()
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
#' # ====================
#' # Normalize intensity
#' # ====================
#'
#' # pmin = 15 because it is an extremely small file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' sensor <- track_sensor(las, Roussel2020(pmin = 15))
#'
#' # Here the effect is virtually null because the size of
#' # the sample is too small to notice any effect of range
#' las <- normalize_intensity(las, range_correction(sensor, Rs = 2000))
NULL
