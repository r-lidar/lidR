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
#' Allows the user to set and examine a variety of global options that affect the way in which
#' lidR computes and displays its results.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{verbose} (\code{logical}) Make the package "talkative". }
#'  \item{\code{progress} (\code{logical}) Display progress bar when available. }
#'  \item{\code{debug} (\code{logical}) Switch the package to debug mode when available.}
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
  progress = TRUE,
  memory_limit_warning = 5e8,
  tiling_size = 1000,

  .allowed = list(
    return_virtual_raster  = bool(),
    buffer = settings::inrange(0, Inf),
    by_file = bool(),
    multicore = settings::inrange(1, Inf),
    progress = bool(),
    memory_limit_warning = settings::inrange(0, Inf),
    tiling_size = settings::inrange(0, Inf)
  )
)

#' Options Settings for the \link{catalog} tools
#'
#' Allow the user to set and examine a variety of global options that affect the way in which
#' lidR processes an entire catalog.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set
#' options.
#'
#' @section Supported options:
#' The following options are supported:
#' \itemize{
#'  \item{\code{progress} (\code{logical}) Display progress bar. Default is TRUE. }
#'  \item{\code{buffer} (numeric) - When applying a function to an entire catalog
#'  sequentially processing sub-areas (clusters) some algorithms (such as \link{grid_terrain})
#'  require a buffer around the area to avoid edge effects. Default is 15 m.}
#'  \item{\code{multicore} (numeric) - For parallel processes, fix the number of
#'  cores to use. Default is the number of cores you have.}
#'  \item{\code{tiling_size} (numeric) - To process an entire catalog, the algorithm splits the
#'  dataset into several square sub-areas (clusters) to process them sequentially. This is the
#'  size of each square cluster. Default is 1000 (1 km^2).}
#'  \item{\code{by_file} (logical) - This option overwrites the option \code{tiling_size}. Instead
#'  of processing the catalog by arbitrary split areas, it forces processing by file. Buffering
#'  is still available.}
#'  \item{\code{return_virtual_raster} (logical) - Functions which return raster-like
#'  data such as \link{grid_metrics}, \link{grid_terrain} and other \code{grid_*} functions
#'  may return huge amounts of data for large catalogs or high resolution data (typically
#'  \code{grid_terrain} with a resolution of 1 meter). Switching this option to \code{TRUE}
#'  enables storage of the data on the hard disk and returns a lightweight virtual raster mosaic.}
#'  \item{\code{memory_limit_warning} (numeric) - When applying a function to an entire
#'  catalog, an internal function tries to estimate the size of the output before running the
#'  algorithm in an attempt to prevent memory overflow. This value (in bytes) is the threshold
#'  before a warning is given. Default is 5e8 (500 Mb). Set to \code{Inf} to disable.}
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
