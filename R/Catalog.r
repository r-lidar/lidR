#' Build a Catalog object
#'
#' Methods to creates a \code{Catalog} object from a folder name
#'
#' A catalog is the representation of a set of las files. A computer cannot load all
#' the data at the same time. A catalog is a simple way to manage all the file sequentially
#' reading only the headers.
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Unused
#' @seealso
#' \link[lidR:Catalog]{Catalog-class}
#' \link[lidR:plot.Catalog]{plot}
#' \link[lidR:processParallel]{processParallel}
#' \link[lidR:extractGroundInventory]{extractGroundInventory}
#' @return A Catalog object
#' @export Catalog
Catalog <- function(folder, ...) {return(new("Catalog", folder, ...))}