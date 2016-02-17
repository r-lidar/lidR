setClass(
	Class = "LiDARProject",
	representation = representation(
		headers = "data.frame"
	)
)

#' Build a LiDARProject object
#'
#' @param folder string. The path of a folder containing a set of .las files
#' @param \dots Unused
#' @return A LiDARProject object
#' @export LiDARProject
LiDARProject <- function(folder, ...) {return(new("LiDARProject", folder, ...))}

setMethod("initialize", "LiDARProject",
	function(.Object, folder, ...)
	{
	  if(!is.character(folder))
	    stop("Invalid parameter input in constructor. Expected string.")

	  if(!dir.exists(folder))
	    stop("This folder doesn't exist.")

	  files = list.files(folder, full.names = T)

	  headers = list()

	  for(file in files)
	  {
	    h = readLASheader(file)

	    h$`Generating Software` = rawToChar(h$`Generating Software` )
	    h$`System Identifier` = rawToChar(h$`System Identifier`)

	    h$`Version Major` = NULL
	    h$`Version Minor` = NULL
	    h$`Project ID - GUID data 1` = NULL
	    h$`Project ID - GUID data 2` = NULL
	    h$`Project ID - GUID data 3` = NULL
	    h$`Project ID - GUID data 4` = NULL
	    h$`Number of points by return` = NULL
	    h$`Point Data Format ID (0-99 for spec)` = NULL

	    headers[[file]] = h
	  }

	  headers = do.call(rbind.data.frame, headers)
	  headers$filename = files
	  rownames(headers) <- NULL

	  .Object@headers = headers

	  return(.Object)
	}
)

#' Plot a LiDARProject object
#'
#' @param x A LiDARProject object
#' @param y Unused (inherited from base plot)
#' @param \dots Unused (inherited from base plot)
#' @export plot
setMethod("plot", "LiDARProject",
	function(x, y, ...)
	{
      headers = x@headers

      xmin = min(headers$Min.X)
      xmax = max(headers$Max.X)
      ymin = min(headers$Min.Y)
      ymax = max(headers$Max.Y)

      plot(0,0, xlim=c(xmin, xmax), ylim = c(ymin, ymax), col="white", asp=1, xlab="X", ylab="Y")

     for(i in 1:dim(headers)[1])
     {
       tile = headers[i,]

       xmax = tile$Max.X[1]
       ymax = tile$Max.Y[1]
       xmin = tile$Min.X[1]
       ymin = tile$Min.Y[1]

       rect(xmin, ymin, xmax, ymax)
     }
	}
)

#' Apply a function to a set of tiles
#'
#' Apply a function to a set of tiles using several cores
#'
#' @aliases processParallel
#' @param x  A LiDARProject object
#' @param func A function which have one parameter: the name of a .las file
#' @param mc.cores numeric. Number of core used. Default is "auto"
#' @param combine character. The function used to merge the outputs of the \code{func} function
#' @param \dots Other parameters for \code{mclapply}
#' @seealso
#' \link[parallel:mclapply]{mclapply}
#' @export processParallel
#' @importFrom parallel mclapply detectCores
setGeneric("processParallel", function(x, func, mc.cores = "auto", combine = "rbind", ...){standardGeneric("processParallel")})
setMethod("processParallel", "LiDARProject",
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

#  ========= EN DEVELOPPEMENT =========

#' Find the tiles containing plots
#'
#' @aliases retrieveInventoryTiles
#' @param obj A LiDARProject object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radiuses of plots
#' @param buffer numeric. A buffer value to expend the extent of search
#' @export retrieveInventoryTiles
setGeneric("retrieveInventoryTiles", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("retrieveInventoryTiles")})
setMethod("retrieveInventoryTiles", "LiDARProject",
	function(obj, plotnames, x, y, radius, buffer = 2)
	{
	    X <- Y <- tile <- minx <- maxx <- miny <- maxy <- NULL # for RMD check

      coord.tiles = data.table(
        tile = obj@headers$filename,
        minx = obj@headers$Min.X,
        maxx = obj@headers$Max.X,
        miny = obj@headers$Min.Y,
        maxy = obj@headers$Max.Y)

      coord.plot = data.table(plotnames = plotnames, X = x, Y = y, radius = radius)

      coord.plot %<>% mutate(maxx = X+radius,
                             maxy = Y+radius,
                             minx = X-radius,
                             miny = Y-radius)

      coord.plot %<>% mutate(tile1 = NA_character_,
                             tile2 = NA_character_,
                             tile3 = NA_character_,
                             tile4 = NA_character_)

      cat("\nLooking for tiles containing plot inventories...\n")

      p <- progress_estimated(length(coord.plot$X))

      for(i in 1:length(coord.plot$X))
      {
        coord = coord.plot[i]
        tiles = filter(coord.tiles,
              (between(coord$minx, minx, maxx) & between(coord$miny, miny, maxy))|
              (between(coord$maxx, minx, maxx) & between(coord$miny, miny, maxy))|
              (between(coord$maxx, minx, maxx) & between(coord$maxy, miny, maxy))|
              (between(coord$minx, minx, maxx) & between(coord$maxy, miny, maxy)))$tile

        coord.plot[i]$tile1 = tiles[1]
        coord.plot[i]$tile2 = tiles[2]
        coord.plot[i]$tile3 = tiles[3]
        coord.plot[i]$tile4 = tiles[4]

        p$tick()$print()
      }

      coord.plot[,c("maxx", "maxy", "minx", "miny"):=NULL]

      return(coord.plot)
	}
)

#' Extract inventory from a set of tiles
#'
#' @aliases extractGroundInventory
#' @param obj A LiDARProject object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radiuses of plots
#' @param buffer numeric. A buffer value to expend the extent of search
#' @return A list of Lidar objects
#' @export extractGroundInventory
setGeneric("extractGroundInventory", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("extractGroundInventory")})
setMethod("extractGroundInventory", "LiDARProject",
  function(obj, plotnames, x, y, radius, buffer = 2)
  {
    tile1 <- tile2 <- tile3 <- tile4 <- NULL

    nplot = length(plotnames)

    tilesForPlots = obj %>% retrieveInventoryTiles(plotnames, x, y, radius, buffer)

    tilesForPlots %<>% group_by(tile1, tile2, tile3, tile4) %>%
      summarise(plotnames = list(plotnames), X = list(X), Y = list(Y), radius = list(radius)) %>%
      ungroup

    output = vector("list", nplot)

    cat("\nExtracting plot inventories...\n")

    p <- progress_estimated(nplot)

    k = 1

    for(i in 1:dim(tilesForPlots)[1])
    {
      line   = tilesForPlots[i]

      file1  = line$tile1
      file2  = line$tile2
      file3  = line$tile3
      file4  = line$tile4

      files  = c(file1, file2, file3, file4)
      files  = files[!is.na(files)]

      names  = line$plotnames[[1]]
      X      = line$X[[1]]
      Y      = line$Y[[1]]
      radius = line$radius[[1]]

      lidar  = Lidar(files)

      for(j in 1:length(names))
      {
        output[[k]] = clipCircle(lidar, X[j], Y[j], radius[j])
        k = k+1
        p$tick()$print()
      }
    }

    names(output) = plotnames

    return(output)
  }
)

# =====================================
