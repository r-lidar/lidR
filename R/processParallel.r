#' Apply a function to a set of tiles
#'
#' Apply a function to a set of tiles using several cores (Linux only, Windows users can only use one core, sorry...)
#'
#' When users have a set of LAS data organized in several tiles it can apply a user function to each tile.
#' This function describes the procedure to apply to each file beginning with data loading (see example).
#' @aliases processParallel
#' @param x  A Catalog object
#' @param func A function which has one parameter: the name of a .las file
#' @param mc.cores numeric. Number of cores used. Default is "auto"
#' @param combine character. The function used to merge the outputs of the \code{func} function
#' @param \dots Other parameters for \code{mclapply}
#' @examples
#' \dontrun{
#' # 1. build a project
#' project = Catalog("folder")
#' plot(project)
#'
#' # 2. load the shapefile you need to filter your points (if needed).
#' lake = rgdal::readOGR("folder", "shapefile")
#'
#' # 3 build the function which analyses a tile (a file).
#' # This function input is only the path of a .las file
#' # see the following template
#'
#' analyse_tile = function(LASFile)
#' {
#'   # Load the data
#'   lidar = readLAS(LASFile)
#'
#'   # Associate geographic data with lidar points (if needed)
#'   lidar %<>% classifyFromShapefile(lake, field="inlake")
#'
#'   # filter lake
#'   lidar %<>% extract(lake == FALSE)
#'   # compute all metrics
#'   metrics = gridMetrics(lidar, 20, myMetrics(X,Y,Z,Intensity,ScanAngle,pulseID))
#'
#'   return(metrics)
#' }
#'
#' # 5. Process the project. By default it detects how many cores you have. But you can add
#' # an optional parameter mc.core = 3. see ?mclapply for other options
#' output = project %>% processParallel(analyse_tile)
#' }
#' @seealso
#' \link[lidR:Catalog-class]{catalog}
#' \link[parallel:mclapply]{mclapply}
#' \link[lidR:classifyFromShapefile]{classifyFromShapefile}
#' \link[lidR:gridMetrics]{gridMetrics}
#' @export processParallel
#' @importFrom parallel mclapply detectCores
setGeneric("processParallel", function(x, func, mc.cores = "auto", combine = "rbind", ...){standardGeneric("processParallel")})

#' @rdname processParallel
setMethod("processParallel", "Catalog",
	function(x, func, mc.cores = "auto", combine = "rbind", ...)
	{
	    if(mc.cores == "auto")
	      mc.cores = parallel::detectCores()
	    else
	      mc.cores = mc.cores

	    cat("Begin parallel processing... \n\n")

      ti = Sys.time()

      files = x@headers$filename

      out = parallel::mclapply(files, func, mc.preschedule = FALSE, mc.cores = mc.cores, ...)

      out = do.call(combine, out)

      tf = Sys.time()

      cat("Process done in", round(difftime(tf, ti, units="min"), 1), "min\n\n")

      return(out)
	}
)
