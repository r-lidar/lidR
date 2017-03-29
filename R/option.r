
LIDROPTIONS <- settings::options_manager(verbose = FALSE, debug = FALSE,
                                         .allowed = list(verbose = settings::inlist(TRUE, FALSE),
                                                         debug   = settings::inlist(TRUE, FALSE)))

#' Set or get global options for lidR package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{verbose}}{(\code{logical}) Make the package talkative. }
#'  \item{\code{debug}}{(\code{logical}) If you encounter a bug maybe that option would enable to find the problem more easily. }
#' }
#'
#' @export
lidr_options <- function(...)
{
  settings::stop_if_reserved(...)
  LIDROPTIONS(...)
}

#' Reset global options for lidR package
#'
#' @export
lidR_reset = function() { settings::reset(LIDROPTIONS) }