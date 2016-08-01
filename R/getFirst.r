#' Filter first returns
#'
#' Select only the first returns.
#'
#' @aliases  getFirst
#' @param obj An object of class \code{LAS}
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' firstReturns = lidar %>% getFirst
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getFirst
#' @note \code{getFirst(obj)} is an alias for \code{extract(obj, ReturnNumber == 1)}
setGeneric("getFirst", function(obj){standardGeneric("getFirst")})


#' @rdname getFirst
setMethod("getFirst", "LAS",
	function(obj)
	{
		return(getNth(obj, 1))
	}
)
