#' @section Working with a \code{LAScatalog}:
#' This section appears in each function that supports a LAScatalog as input.\cr
#'
#' In \code{lidR} when the input of a function is a \link[lidR:LAScatalog-class]{LAScatalog} the
#' function uses the LAScatalog processing engine. The user can modify the engine options using
#' the \link[lidR:catalog_options_tools]{available options}. A careful reading of the
#' \link[lidR:LAScatalog-class]{engine documentation} is recommended before processing \code{LAScatalogs}. Each
#' \code{lidR} function should come with a section that documents the supported engine options.\cr
#'
#' The \code{LAScatalog} engine supports \code{.lax} files that \emph{significantly} improve the computation
#' speed of spatial queries using a spatial index. Users should really take advange a \code{.lax} files,
#' but this is not mandatory.\cr
#'
#' More documentation is avaible in the vignettes: \code{browseVignettes("lidR")}
