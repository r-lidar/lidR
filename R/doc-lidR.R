#' lidR: airborne LiDAR for forestry application
#'
#' \if{html}{\figure{logo.png}{options: align='right'}} lidR provides a set of tools to manipulate
#' airborne LiDAR data in forestry contexts. The package works essentially with .las or .laz files.
#' The toolbox includes algorithms for DSM, CHM, DTM, ABA, normalisation, tree detection, tree
#' segmentation and other tools as well as an engine to process wide LiDAR coverages split into many
#' files.
#'
#' To learn more about lidR, start with the vignettes: browseVignettes(package = "lidR"). Users can also
#' find non official supplementary documentation in the \href{https://github.com/Jean-Romain/lidR/wiki}{github wiki pages}.
#' To ask "how to" questions please ask on \href{https://gis.stackexchange.com/}{gis.stackexchange.com}
#' with the tag \code{lidr}.
#'
#' @section Package options:
#' \describe{
#' \item{\code{lidR.progress}}{Several functions have a progress bar for long operations (but not all).
#' Should lengthy operations show a progress bar? Default: TRUE}
#' \item{\code{lidR.progress.delay}}{The progress bar appears only for long operations. After how many seconds
#' of computation the progress bar appears? Default: 2}
#' \item{\code{lidR.verbose}}{Make the package verbose. Default: FALSE}
#' \item{\code{lidR.buildVRT}}{The functions \code{grid_*} can write the rasters sequentially on the
#' disk and load back a virtual raster mosaic (VRT) instead of the list of written files. Should
#' a VRT been built? Default: TRUE}
#' }
#'
#' @useDynLib lidR, .registration = TRUE
#' @import data.table
#' @import methods
#' @importClassesFrom sp Spatial
"_PACKAGE"
