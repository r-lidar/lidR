
LIDROPTIONS <- settings::options_manager(verbose = FALSE,
                                         progress = FALSE,
                                         debug = FALSE,
                                         .allowed = list(verbose  = settings::inlist(TRUE, FALSE),
                                                         debug    = settings::inlist(TRUE, FALSE),
                                                         progress = settings::inlist(TRUE, FALSE)))

#' Set or get global options for lidR package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{verbose} (\code{logical}) Make the package talkative. }
#'  \item{\code{progress} (\code{logical}) Display progress bars when avaible. }
#'  \item{\code{debug} (\code{logical}) Turn the package to debug mode when avaible.}
#' }
#'
#' @examples
#' lidr_options(verbose = TRUE)
#' lidr_options(progress = TRUE)
#' lidr_options()
#'
#' # Reset default options
#' lidr_reset()
#' @export
lidr_options <- function(...)
{
  settings::stop_if_reserved(...)
  LIDROPTIONS(...)
}

#' @export
#' @rdname lidr_options
lidr_reset = function() { settings::reset(LIDROPTIONS) }


CATALOGOPTIONS <- settings::options_manager(return_virtual_raster = FALSE,
                                            buffer = 30,
                                            by_file = FALSE,
                                            multicore = parallel::detectCores(),
                                            memory_limit_warning = 250e6,
                                            tiling_size = 1000)

#' Set or get global options for \link{catalog} tools
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set
#' options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{return_virtual_raster} (logical) functions which return raster-alike
#'  data such as \link{grid_metrics}, \link{grid_terrain} and other \code{grid_*} functions
#'  may return huge amount of data for large catalog or hight resolution (typically
#'  \code{grid_terrain} with a resolution of 1 meter). Switching this options enable to
#'  store the data on the hard disk and return a light weight virtual raster mosaic.}
#'  \item{\code{buffer} (numeric) when applying a function to an entiere catalog
#'  processing sequentially sub-areas some algotihms (such as \link{grid_terrain}) requiere
#'  a buffer around the area. Default is 30 m.}
#'  \item{\code{multicore} (numeric) for parallel processes, fix the number of
#'  core to use. Default is the number of core you have.}
#'  \item{\code{memory_limit_warning} (numeric) when applying a function to an entiere
#'  catalog, an internal function try to estimate the size of the output before to run the
#'  algorithm in attempt to prevent memory overflow. This value (in bytes) is the threshold
#'  before to get a warning. Set to \code{Inf} to disable.},
#'  \item{\code{tiling_size} (numeric) to process an entiere catalog, the algorithm split the
#'  dataset in several squqre subareas to process them sequentially. This is the size of a square.
#'  Defaul is 1000 (1 km^2).}
#'  }
#'
#' @examples
#' catalog_options(multicore = 2)
#' catalog_options(buffer = 40)
#' catalog_options()
#'
#' # Reset default options
#' catalog_reset()
#' @export
catalog_options <- function(...)
{
  settings::stop_if_reserved(...)
  CATALOGOPTIONS(...)
}

#' @export
#' @rdname catalog_options
catalog_reset = function() { settings::reset(CATALOGOPTIONS) }
