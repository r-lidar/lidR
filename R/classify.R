#' Classify points
#'
#' Classify points that meet some criterion and/or that belong in a region of interest. The
#' functions updates the attribute `Classification` of the LAS object according to
#' \href{https://www.asprs.org/divisions-committees/lidar-division/laser-las-file-format-exchange-activities}{las specifications}
#'
#' \describe{
#' \item{classify_noise}{Classify points as 'noise' (outliers) with several possible algorithms.
#' lidR has: \link{sor}, \link{ivf}. The points classified as 'noise' are assigned a value of 18.}
#' \item{classify_ground}{Classify points as 'ground' with several possible algorithms.
#' lidR has \link{pmf}, \link{csf} and \link{mcc}. The points classified as 'ground' are assigned a
#' value of 2 }
#' \item{classify_poi}{Classify points that meet some logical criterion and/or that belong in a
#' region of interest with class of choice.}
#' }
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file format
#' and all the attributes. `select = "*"` is imposed internally.
#'
#' @template param-las
#' @param algorithm An algorithm for classification. lidR has has: \link{sor}, \link{ivf} for noise
#' classification, and \link{pmf}, \link{csf}, \link{mcc} for ground classification (see respective
#' documentation).
#' @param class The ASPRS class to attribute to the points that meet the criterion.
#' @param poi a formula of logical predicates. The points that are `TRUE` will be classified `class`.
#' @param roi A `SpatialPolygons*`, from `sp` or a `sf/sfc_POLYGON` from `sf`.
#' The points that are in the region of interest delimited by the polygon(s) are classified
#' `class`.
#' @param inverse_roi bool. Inverses the `roi`. The points that are outside the polygon(s)
#' are classified `class`.
#' @param by_reference bool. Updates the classification in place (LAS only).
#' @param last_returns logical. The algorithm will use only the last returns (including the first returns
#' in cases of a single return) to run the algorithm. If FALSE all the returns are used. If the attributes
#' \code{'ReturnNumber'} or \code{'NumberOfReturns'} are absent, \code{'last_returns'} is turned
#' to \code{FALSE} automatically.
#'
#' @name classify
#' @rdname classify
#' @md
#' @examples
#' # ===============
#' # Classify ground
#' # ===============
#'
#' if (require(RCSF, quietly = TRUE))
#' {
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn", filter = "-inside 273450 5274350 273550 5274450")
#'
#' # (Parameters chosen mainly for speed)
#' mycsf <- csf(TRUE, 1, 1, time_step = 1)
#' las <- classify_ground(las, mycsf)
#' #plot(las, color = "Classification")
#' }
#'
#' # ===============
#' # Classify noise
#' # ===============
#'
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#' # Add 20 artificial outliers
#' set.seed(314)
#' id = round(runif(20, 0, npoints(las)))
#' set.seed(42)
#' err = runif(20, -50, 50)
#' las$Z[id] = las$Z[id] + err
#'
#' # Using IVF
#' las <- classify_noise(las, ivf(5,2))
#' #plot(las, color = "Classification")
#'
#' # Remove outliers using filter_poi()
#' las_denoise <- filter_poi(las, Classification != LASNOISE)
#'
#' # ===============
#' # Classify POI
#' # ===============
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shp <- system.file("extdata", "lake_polygons_UTM17.shp", package = "lidR")
#'
#' las  <- readLAS(LASfile, filter = "-keep_random_fraction 0.1")
#' lake <- sf::st_read(shp, quiet = TRUE)
#'
#' # Classifies the points that are NOT in the lake and that are NOT ground points as class 5
#' poi <- ~Classification != LASGROUND
#' las <- classify_poi(las, LASHIGHVEGETATION, poi = poi, roi = lake, inverse = TRUE)
#'
#' # Classifies the points that are in the lake as class 9
#' las <- classify_poi(las, LASWATER, roi = lake, inverse = FALSE)
#'
#' #plot(las, color = "Classification")
NULL
