#' Filter first returns from pulses which returned multiple points
#'
#' Select only the first returns from pulses which returned multiple points
#'
#' @aliases  getFirstOfMany
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' firstOfManyReturns = lidar %>% getFirstOfMany
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getFirstOfMany
#' @note \code{getFirstOfMany(obj)} is an alias for \code{extract(obj, NumberOfReturns > 1, ReturnNumber == 1))}
setGeneric("getFirstOfMany", function(obj){standardGeneric("getFirstOfMany")})

#' @rdname getFirstOfMany
setMethod("getFirstOfMany", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(extract(obj, NumberOfReturns > 1, ReturnNumber == 1))
	}
)