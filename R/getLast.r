#' Filter last returns
#'
#' Select only the last returns i.e. the last returns and the single returns
#' @aliases  getLast
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' lastReturns = lidar %>% getLast
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getLast
#' @note \code{getLast(obj)} is an alias for \code{extract(obj, ReturnNumber == NumberOfReturns))}
setGeneric("getLast", function(obj){standardGeneric("getLast")})

#' @rdname  getLast
setMethod("getLast", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(extract(obj, ReturnNumber == NumberOfReturns))
	}
)