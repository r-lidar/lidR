#' Rasterize a point cloud
#'
#' Rasterize a point cloud in different ways to compute a DTM, a CHM or a density map. Most
#' raster products can be computed with \link{pixel_metrics} but some are more complex and require
#' dedicated and optimized functions. See Details and Examples.
#'
#' \describe{
#' \item{`rasterize_terrain`}{Interpolates the ground points and creates a rasterized
#' digital terrain model. The algorithm uses the points classified as "ground" and "water"
#' (Classification = 2 and 9, respectively, according to
#' \href{https://community.asprs.org/leadership-restricted/leadership-content/public-documents/standards}{LAS file format specifications})
#' to compute the interpolation. How well the edges of the dataset are interpolated depends on the
#' interpolation method used. A buffer around the region of interest is always recommended to avoid
#' edge effects.}
#' \item{`rasterize_canopy`}{Creates a digital surface model (DSM) using several
#' possible algorithms. If the user provides a normalized point cloud, the output is indeed a canopy
#' height model (CHM).}
#' \item{`rasterize_density`}{Creates a map of the point density. If a "pulseID"
#' attribute is found, also returns a map of the pulse density.}
#' }
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected in `rasterize_*` because it is internally
#' known what is best to select.\cr
#' The option `chunk_buffer` is not supported and not respected in `rasterize_canopy` and
#' `rasterize_density` because it is not necessary.
#'
#' @template param-las
#' @param algorithm function. A function that implements an algorithm to compute a digital surface model
#' or a digital terrain model. \code{lidR} implements \link{p2r}, \link{dsmtin}, \link{pitfree}
#' for digital surface models, and \link{knnidw}, \link{tin}, and \link{kriging} for digital terrain
#' models (see respective documentation and examples).
#' @param ... Use `pkg = "terra|raster|stars"` to get an output in `SpatRaster`, `RasterLayer`
#' or `stars` format. Default is `getOption("lidR.raster.default")`.
#' @param res numeric. The size of a grid cell in point cloud coordinates units. Can also be
#'  `RasterLayer` or a `stars` or a `SpatRaster` used as layout.
#' @param shape By default the interpolation is made only within the `"convex"` hull of
#' the point cloud to get a DTM with the shape of the point cloud. This prevents meaningless
#' interpolations where there is no data. It can also be `"concave"` or `"bbox"`. It can also be an `sfc`
#' to define a polygon in which to perform the interpolation.
#' @param use_class integer vector. By default the terrain is computed by using ground points
#' (class 2) and water points (class 9).
#'
#' @return `RasterLayer` or a `stars` or a `SpatRaster` depending on the settings.
#' @name rasterize
#' @rdname rasterize
#' @md
#' @examples
#'
#' # =====================
#' # Digital Terrain Model
#' # =====================
#'
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#' #plot(las)
#'
#' dtm1 = rasterize_terrain(las, algorithm = knnidw(k = 6L, p = 2))
#' dtm2 = rasterize_terrain(las, algorithm = tin())
#'
#' \dontrun{
#' dtm3 = rasterize_terrain(las, algorithm = kriging(k = 10L))
#'
#' plot(dtm1, col = gray(0:25/25))
#' plot(dtm2, col = gray(0:25/25))
#' plot(dtm3, col = gray(0:25/25))
#' plot_dtm3d(dtm1)
#' plot_dtm3d(dtm2)
#' plot_dtm3d(dtm3)
#' }
#'
#' # =====================
#' # Digital Surface Model
#' # =====================
#'
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-inside 481280 3812940 481330 3812990")
#' col <- height.colors(15)
#'
#' # Points-to-raster algorithm with a resolution of 1 meter
#' chm <- rasterize_canopy(las, res = 1, p2r())
#' plot(chm, col = col)
#'
#' # Points-to-raster algorithm with a resolution of 0.5 meters replacing each
#' # point by a 20-cm radius circle of 8 points
#' chm <- rasterize_canopy(las, res = 0.5, p2r(0.2))
#' plot(chm, col = col)
#'
#' # Basic triangulation and rasterization of first returns
#' chm <- rasterize_canopy(las, res = 0.5, dsmtin())
#' plot(chm, col = col)
#'
#' # Khosravipour et al. pitfree algorithm
#' chm <- rasterize_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
#' plot(chm, col = col)
#'
#' # ====================
#' # Digital Density Map
#' # ====================
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile,  filter = "-inside 684800 5017800 684900 5017900")
#'
#' d <- rasterize_density(las, 5)
#' plot(d)
#'
#' las <- retrieve_pulses(las)
#' d <- rasterize_density(las)
#' plot(d)
NULL
