#' Filter single returns
#'
#' Select only the returns which return only one point.
#'
#' @aliases  getSingle
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' singleReturns = lidar %>% getSingle
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getSingle
#' @note \code{getSingle(obj)} is an alias for \code{extract(obj, NumberOfReturns == 1))}
setGeneric("getSingle", function(obj){standardGeneric("getSingle")})

#' @rdname getSingle
setMethod("getSingle", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- NULL

		return(extract(obj, NumberOfReturns == 1))
	}
)