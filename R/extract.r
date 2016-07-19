#' Return points with matching conditions
#'
#' Return points with matching conditions. \code{extract} is an overloading
#' function for \code{Lidar} objects which replaces the function
#' \code{\link[dplyr:filter]{filter}} from \code{\link[dplyr:dplyr]{dplyr}} package.
#'
#' @aliases extract
#' @param .data An object of class \code{Lidar}
#' @param \dots Logical predicates. Multiple conditions are combined with &.
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = lidar %>% extract(Classification == 1, ReturnNumber == 1)
#' @seealso
#' \link[dplyr:filter]{filter}
#' \link[lidR:Lidar]{Class Lidar}
#' \link[lidR:getFirst]{getFirst}
#' \link[lidR:getFirstLast]{getFirstLast}
#' \link[lidR:getFirstOfMany]{getFirstOfMany}
#' \link[lidR:getSingle]{getSingle}
#' \link[lidR:getLast]{getLast}
#' \link[lidR:getGround]{getGround}
#' \link[lidR:getNth]{getNth}
#' @export extract
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
setGeneric("extract", function(.data, ...){standardGeneric("extract")})

#' @rdname extract
setMethod("extract", "Lidar",
	function(.data, ...)
	{
		ret = .data@data %>% dplyr::filter(...) %>% LoadLidar

		return(ret)
	}
)
