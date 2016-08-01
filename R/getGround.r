#' Filter returns classified as ground
#'
#' Select only the returns classified as ground according to LAS specification v1.3.
#'
#' @aliases  getGround
#' @param obj An object of class \code{LAS}
#' @return An object of class \code{LAS}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = readLAS(LASfile)
#'
#' ground = lidar %>% getGround
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:extract]{extract} }
#' @export getGround
#' @note \code{getGround(obj)} is an alias for \code{extract(obj, Classification == 2)}
setGeneric("getGround", function(obj){standardGeneric("getGround")})

#' @rdname getGround
setMethod("getGround", "LAS",
	function(obj)
	{
	  Classification <- NULL

	 	return(extract(obj, Classification == 2))
 	}
)
