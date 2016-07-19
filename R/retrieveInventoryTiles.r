#' Retrieve the tiles containing plots
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plots automatically. This function retrieves the tiles
#' for each plot. This function is used by \link[lidR:extractGroundInventory]{extractGroundInventory}.
#' Users do not really need it.
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plots automatically. The algorithm is able to find tiles
#' for plots falling between several tiles (on the edges) if tiles are organized in a grid.
#'
#' @aliases retrieveInventoryTiles
#' @param obj A Catalog object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radii of plots
#' @param buffer numeric. A buffer value to extend the search range
#' @export retrieveInventoryTiles
#' @importFrom dplyr mutate progress_estimated
#' @importFrom data.table :=
setGeneric("retrieveInventoryTiles", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("retrieveInventoryTiles")})

#' @rdname retrieveInventoryTiles
setMethod("retrieveInventoryTiles", "Catalog",
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

      coord.plot %<>% dplyr::mutate(maxx = X+radius,
                             maxy = Y+radius,
                             minx = X-radius,
                             miny = Y-radius)

      coord.plot %<>% dplyr::mutate(tile1 = NA_character_,
                             tile2 = NA_character_,
                             tile3 = NA_character_,
                             tile4 = NA_character_)

      cat("\nLooking for tiles containing plot inventories...\n")

      p <- dplyr::progress_estimated(length(coord.plot$X))

      for(i in 1:length(coord.plot$X))
      {
        coord = coord.plot[i]
        tiles = dplyr::filter(coord.tiles,
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