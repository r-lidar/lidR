#' An S4 class to represent a catalog of las tiles.
#'
#' An S4 class to represent a set of las tiles, to plot them and to process them with
#' multicore.
#'
#' A \code{Catalog} object contains a \code{data.frame} in the slot \code{@headers} with the data
#' read from the headers of all user's \code{.las} files. A catalog is the representation
#' of a set of las files. A computer cannot load all the data at thet same time. A catalog
#' is a simple way to manage all the files sequentially reading only the headers. See the
#' public documentation of las format for more information.
#' @slot headers data.frame. A table representing the las header data
#' @name Catalog-class
#' @rdname Catalog-class
#' @exportClass Catalog
#' @seealso
#' \link[lidR:Catalog]{Catalog}
#' \link[lidR:plot.Catalog]{plot}
#' \link[lidR:processParallel]{processParallel}
#' \link[lidR:extractGroundInventory]{extractGroundInventory}
setClass(
	Class = "Catalog",
	representation = representation(
		headers = "data.frame"
	)
)

setMethod("initialize", "Catalog",
	function(.Object, folder, ...)
	{
	  if(!is.character(folder))
	     lidRError("GTG1")

	  if(!dir.exists(folder))
	     lidRError("CTG2")

	  files = list.files(folder, full.names = T)

	  headers = lapply(files, readLASheader)
	  headers = do.call(rbind.data.frame, headers)
	  headers$filename = files
	  rownames(headers) <- NULL

	  .Object@headers = headers

	  return(.Object)
	}
)





