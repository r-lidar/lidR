bool = function() { settings::inlist(TRUE, FALSE) }

LIDROPTIONS <- settings::options_manager(
  verbose = FALSE,
  progress = FALSE,
  debug = FALSE,
  interactive = TRUE,

  .allowed = list(
    verbose  = bool(),
    debug    = bool(),
    progress = bool(),
    interactive = bool()
  )
)

#' Options Settings for the lidR package
#'
#' Allow the user to set and examine a variety of global options that affect the way in which
#' lidR computes and displays its results.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{verbose} (\code{logical}) Make the package "talkative". }
#'  \item{\code{progress} (\code{logical}) Display progress bars when available. }
#'  \item{\code{debug} (\code{logical}) Turn the package to debug mode when available.}
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
#' @seealso \link{catalog_options}
lidr_options <- function(...)
{
  settings::stop_if_reserved(...)
  LIDROPTIONS(...)
}

#' @export
#' @rdname lidr_options
lidr_reset = function() { settings::reset(LIDROPTIONS) }


CATALOGOPTIONS <- settings::options_manager(
  return_virtual_raster = FALSE,
  buffer = 15,
  by_file = FALSE,
  multicore = parallel::detectCores(),
  memory_limit_warning = 5e8,
  tiling_size = 1000,

  .allowed = list(
    return_virtual_raster  = bool(),
    buffer = settings::inrange(0, Inf),
    by_file = bool(),
    multicore = settings::inrange(1, Inf),
    memory_limit_warning = settings::inrange(0, Inf),
    tiling_size = settings::inrange(0, Inf)
  )
)

#' Options Settings for the \link{catalog} tools
#'
#' Allow the user to set and examine a variety of global options that affect the way in which
#' lidR process an entire catalog.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set
#' options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{buffer} (numeric) - When applying a function to an entiere catalog
#'  processing sequentially sub-areas (clusters) some algotihms (such as \link{grid_terrain})
#'  requiere a buffer around the area to avoid edge effects. Default is 15 m.}
#'  \item{\code{multicore} (numeric) - For parallel processes, fix the number of
#'  core to use. Default is the number of core you have.}
#'  \item{\code{tiling_size} (numeric) - To process an entiere catalog, the algorithm split the
#'  dataset in several square sub-areas (clusters) to process them sequentially. This is the
#'  size of each square cluster. Default is 1000 (1 km^2).}
#'  \item{\code{by_file} (logical) - This option overwrite the option \code{tiling_size}. Instead
#'  of processing the catalog by arbitrary split areas, it forces to process by file. Buffering
#'  is still avaible.}
#'  \item{\code{return_virtual_raster} (logical) - Functions which return raster-alike
#'  data such as \link{grid_metrics}, \link{grid_terrain} and other \code{grid_*} functions
#'  may return huge amount of data for large catalog or hight resolution (typically
#'  \code{grid_terrain} with a resolution of 1 meter). Switching this options to \code{TRUE}
#'  enable to store the data on the hard disk and return a light weight virtual raster mosaic.}
#'  \item{\code{memory_limit_warning} (numeric) - When applying a function to an entiere
#'  catalog, an internal function tries to estimate the size of the output before to run the
#'  algorithm in attempt to prevent memory overflow. This value (in bytes) is the threshold
#'  before to get a warning. Default is 5e8 (500 Mb). Set to \code{Inf} to disable.}
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
