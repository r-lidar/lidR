#' Filter returns by their position in the return sequence.
#'
#' Select the returns from their position in the return sequence. Point density
#' pulse density and area are recomputed on the fly
#'
#' @aliases  getNth
#' @param obj An object of class \code{Lidar}
#' @param n numeric. The position in the return sequence
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' secondReturns = lidar %>% getNth(2)
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getNth
#' @note \code{getNth(obj, n)} is an alias for \code{extract(obj, ReturnNumber == n)}
#' @aliases  getNth
setGeneric("getNth", function(obj, n){standardGeneric("getNth")})

#' @rdname getNth
setMethod("getNth", "Lidar",
	function(obj, n)
	{
	   ReturnNumber <- NULL

	  if(n > max(obj@data$ReturnNumber) | n <= 0)
	    lidRError("LDR5")

		return(extract(obj, ReturnNumber == n))
	}
)