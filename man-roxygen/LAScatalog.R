#' @section Working with a \code{LAScatalog}:
#' In \code{lidR} when the input of a function is a \link[lidR:LAScatalog-class]{LAScatalog} the
#' functions uses the internal catalog processing engine. The user can modify the engine options using
#' the \link[lidR:catalog_options_tools]{available options}. A careful read of the engine
#' \link[lidR:LAScatalog-class]{engine documentation} is recommended to process \code{LAScatalogs}. Each
#' function should come with a section that doucment the avaible engine options.\cr\cr
#' \code{LAScatalog} engine supports \code{.lax} files that \strong{significantly} improve the computation
#' speed of spatial queries using a spatial index. Users should really take advange a \code{.lax} files
#' but this is not mandatory.
