
LIDROPTIONS <- settings::options_manager(verbose = FALSE, debug = FALSE, progress = FALSE,
                                         .allowed = list(verbose  = settings::inlist(TRUE, FALSE),
                                                         debug    = settings::inlist(TRUE, FALSE),
                                                         progress = settings::inlist(TRUE, FALSE)))

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
#' }
#'
#' @examples
#' lidr_options(verbose = TRUE)
#' lidr_options(progress = TRUE)
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