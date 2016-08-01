#' Filter first and last returns
#'
#' Select only the first and last returns.
#'
#' @aliases  getFirstLast
#' @param obj An object of class \code{LAS}
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' firstLastReturns = lidar %>% getFirstLast
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getFirstLast
#' @note \code{getFirstLast(obj)} is an alias for \code{extract(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))}
setGeneric("getFirstLast", function(obj){standardGeneric("getFirstLast")})

#' @rdname getFirstLast
setMethod("getFirstLast", "LAS",
	function(obj)
	{
	  ReturnNumber <- NumberOfReturns <- NULL

		return(extract(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
	}
)
