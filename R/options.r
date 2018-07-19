LIDROPTIONS <- settings::options_manager(
  verbose = FALSE,
  progress = FALSE,
  debug = FALSE,
  interactive = TRUE,
  memlimit = 5e8,

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
lidr_options <- function(...)
{
  settings::stop_if_reserved(...)
  LIDROPTIONS(...)
}

#' @export
#' @rdname lidr_options
lidr_reset = function() { settings::reset(LIDROPTIONS) }
