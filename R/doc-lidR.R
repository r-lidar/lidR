#' lidR: airborne LiDAR for forestry applications
#'
#' lidR provides a set of tools to manipulate airborne LiDAR data in forestry contexts. The package
#' works with .las or .laz files. The toolbox includes algorithms for DSM, CHM, DTM, ABA,
#' normalisation, tree detection, tree segmentation, tree delineation, colourization, validation and
#' other tools, as well as a processing engine to process broad LiDAR coverage split into many files.
#'
#' To learn more about lidR, start with the vignettes: browseVignettes(package = "lidR"). Users can also
#' find unofficial supplementary documentation in the \href{https://r-lidar.github.io/lidRbook/}{lidR book}.
#' To ask "how to" questions please ask on \href{https://gis.stackexchange.com/}{gis.stackexchange.com}
#' with the tag \code{lidr}.
#'
#' @section Package options:
#' \describe{
#' \item{\code{lidR.progress}}{Several functions have a progress bar for long operations (but not all).
#' Should lengthy operations show a progress bar? Default: TRUE}
#' \item{\code{lidR.progress.delay}}{The progress bar appears only for long operations. After how many seconds
#' of computation does the progress bar appear? Default: 2}
#' \item{\code{lidR.raster.default}}{The functions that return a raster are raster agnostic meaning
#' that they work either with rasters from packages `raster`, `stars` or `terra`. By default they return
#' rasters from `stars`. Can be one of "raster", "stars" or "terra". Default: "terra"}
#' \item{\code{lidR.check.nested.parallelism}}{The catalog processing engine (\link{catalog_apply})
#' checks the parallel strategy chosen by the user and verify if C++ parallelization with
#' OpenMP should be disabled to avoid nested parallel loops. Default: TRUE. If FALSE
#' the catalog processing engine will not check for nested parallelism
#' and will respect the settings of \link{set_lidr_threads}.}
#' }
#'
#' @useDynLib lidR, .registration = TRUE
#' @import data.table
#' @import methods
"_PACKAGE"
