#' Select LAS files interactively
#'
#' Select a set of LAS tile from a Catalog using interactively the mouse
#'
#' This function enable the user to select a set of las file from a Catalog clicking on the map of the file with the mouse.
#' The selected files turn red on the plot at the end of the selection.
#' @param x A Catalog object
#' @return A Catalog object
#' @export
#' @examples
#' \dontrun{
#'
#' catalog = Catalog("<Path to a folder containing a set of .las files>")
#' selectedFiles = selectTiles(catalog)
#' }
#' @seealso
#' \link[lidR:Catalog-class]{Catalog-class}
#' \link[lidR:Catalog]{Catalog}
#' @importFrom graphics rect identify
#' @importFrom magrittr %$%
setGeneric("selectTiles", function(x){standardGeneric("selectTiles")})

#' @rdname selectTiles
setMethod("selectTiles", "Catalog",
  function(x)
  {
    Min.X <- Min.Y <- Max.X <- Max.Y <- filename <- NULL

    plot(x)

    selected = x@headers %$% graphics::identify((Min.X+Max.X)/2, (Min.Y+Max.Y)/2, plot=F)

    x@headers = x@headers[selected,]

    x@headers %$% graphics::rect(Min.X, Min.Y, Max.X, Max.Y, col="red")

    return(x)
  }
)
