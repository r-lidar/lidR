setClass(
	Class = "Lidar",
	representation(
		data 					= "data.table",
		area 					= "numeric",
		pointDensity 	= "numeric",
		pulseDensity 	= "numeric",
		header        = "list"
	)
)

#' Create a \code{Lidar} object
#'
#' Methods to read and creates a \code{Lidar} object from a .las file or from a data.table
#' @param input character. filename(s) of .las file(s)
#' @param fields character. \code{"minimal"}, \code{"standard"}, \code{"all"}
#' @param \dots Unused
#' @return An object of the class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#' @seealso
#' \link[lidR:Lidar]{Class Lidar}
#' @export Lidar
Lidar <- function(input, fields = "standard", ...) {return(new("Lidar", input, fields, ...))}

setGeneric("leach", function(.data, ...){standardGeneric("leach")})
setGeneric("getNth", function(obj, n){standardGeneric("getNth")})
setGeneric("getFirst", function(obj){standardGeneric("getFirst")})
setGeneric("getFirstOfMany", function(obj){standardGeneric("getFirstOfMany")})
setGeneric("getSingle", function(obj){standardGeneric("getSingle")})
setGeneric("getLast", function(obj){standardGeneric("getLast")})
setGeneric("getFirstLast", function(obj){standardGeneric("getFirstLast")})
setGeneric("getGround", function(obj){standardGeneric("getGround")})
setGeneric("canopyModel", function(obj, resolution = 2, method="local_maximum"){standardGeneric("canopyModel")})
setGeneric("pulseDensity", function(obj, resolution = 4){standardGeneric("pulseDensity")})
setGeneric("getData", function(obj){standardGeneric("getData")})
setGeneric("clipRectangle", function(obj, xleft, ybottom, xright, ytop, inside = TRUE){standardGeneric("clipRectangle")})
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})
setGeneric("clipCircle", function(obj, xcenter, ycenter, radius, inside = TRUE){standardGeneric("clipCircle")})
setGeneric("gridMetrics", function(obj, resolution, func, option = NULL){standardGeneric("gridMetrics")})
setGeneric("cloudMetrics", function(obj, func){standardGeneric("cloudMetrics")})
setGeneric("thin", function(obj, pulseDensity, homogenize = TRUE, resolution = 5){standardGeneric("thin")})
setGeneric("area", function(obj){standardGeneric("area")})
setGeneric("extent", function(x){standardGeneric("extent")})
setGeneric(".pointDensity", function(obj){standardGeneric(".pointDensity")})
setGeneric(".pulseDensity", function(obj){standardGeneric(".pulseDensity")})
setGeneric("pointsInPoly", function(obj, shapefile, colname){standardGeneric("pointsInPoly")})
setGeneric("classifyFromShapefile", function(obj, shapefile, field){standardGeneric("classifyFromShapefile")})

setMethod("initialize", "Lidar",
	function(.Object, input, fields = "standard", ...)
	{
	  gpstime <- NULL

	  if(is.character(input))
	  {
	    .Object@data   = .loadLAS(input, fields)
	    .Object@header = .loadLASheaders(input)
	  }
	  else if(is.data.table(input))
	    .Object@data = input
	  else
	    stop("Invalid parameter input in constructor")

	  negvalues = sum(.Object@data$Z < 0)
	  class0    = sum(.Object@data$Classification == 0)

	  if(negvalues > 0)
	    warning("Dataset may be invalid: ", negvalues, " points below 0 found")

	  if(class0 > 0)
	    warning("Dataset may be invalid: ", class0, " unclassified points found")

	  setorder(.Object@data, gpstime)

	  .Object@area <- area(.Object)

	  if(is.null(.Object@data$pulseID))
	    .Object@data$pulseID <- .IdentifyPulse(.Object@data$ReturnNumber)

	  if(is.null(.Object@data$flightlineID))
	    .Object@data$flightlineID <- .IdentifyFlightlines(.Object@data$gpstime)

	  .Object@pointDensity <- .pointDensity(.Object)
	  .Object@pulseDensity <- .pulseDensity(.Object)

	  return(.Object)
	}
)

#' Return points with matching conditions
#'
#' Return points with matching conditions. \code{leach} is an overloading
#'
#' function for \code{Lidar} objects which replace the function
#' \code{\link[dplyr:filter]{filter}} from \code{\link[dplyr:dplyr]{dplyr}}
#' package.
#' @aliases  leach
#' @param .data An object of class \code{Lidar}
#' @param \dots Logical predicates. Multiple conditions are combined with &.
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = lidar %>% leach(Classification == 1, ReturnNumber == 1)
#' @seealso
#' \code{\link[dplyr:filter]{filter}}
#' @export leach
setMethod("leach", "Lidar",
	function(.data, ...)
	{
		ret = .data@data %>% filter(...) %>% Lidar

		return(ret)
	}
)

#' Filter returns by their position in the return sequence
#'
#' Select the returns from their position in the return sequence form a \code{Lidar} object
#'
#' function for \code{Lidar} objects which replace the function
#' \code{\link[dplyr:filter]{filter}} from \code{\link[dplyr:dplyr]{dplyr}}
#' package.
#' @aliases  getNth
#' @param obj An object of class \code{Lidar}
#' @param n numeric. The position in the return sequence
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getNth
#' @note \code{getNth(obj, n)} is an alias for \code{leach(obj, ReturnNumber == n)}
setMethod("getNth", "Lidar",
	function(obj, n)
	{
	   ReturnNumber <- NULL

	  if(n > max(obj@data$ReturnNumber) | n <= 0)
	    stop("Parameter n of function getNth incorrect")

		return(leach(obj, ReturnNumber == n))
	}
)

#' Filter first returns
#'
#' Select only the first returns form a \code{Lidar} object.
#'
#' @aliases  getFirst
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getFirst
#' @note \code{getFirst(obj)} is an alias for \code{leach(obj, ReturnNumber == 1)}
setMethod("getFirst", "Lidar",
	function(obj)
	{
		return(getNth(obj, 1))
	}
)

#' Filter first returns from pulses which returned multiple points
#'
#' Select only the first returns from pulses which returned multiple points form a \code{Lidar} object
#'
#' @aliases  getFirstOfMany
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getFirstOfMany
#' @note \code{getFirstOfMany(obj)} is an alias for \code{leach(obj, NumberOfReturns > 1, ReturnNumber == 1))}
setMethod("getFirstOfMany", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(leach(obj, NumberOfReturns > 1, ReturnNumber == 1))
	}
)

#' Filter single returns
#'
#' Select only the returns which return only one points form a \code{Lidar} object
#'
#' @aliases  getSingle
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getSingle
#' @note \code{getSingle(obj)} is an alias for \code{leach(obj, NumberOfReturns == 1))}
setMethod("getSingle", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- NULL

		return(leach(obj, NumberOfReturns == 1))
	}
)

#' Filter last returns
#'
#' Select only the last returns form a \code{Lidar} object i.e. the last returns and the single returns
#' @aliases  getLast
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getLast
#' @note \code{getLast(obj)} is an alias for \code{leach(obj, ReturnNumber == NumberOfReturns))}
setMethod("getLast", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(leach(obj, ReturnNumber == NumberOfReturns))
	}
)

#' Filter first and last returns
#'
#' Select only the first and last returns form a \code{Lidar} object
#'
#' @aliases  getFirstLast
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' firstLastReturns = lidar %>% getFirstLast
#' @seealso
#' \code{\link[lidR:getFirst]{getFirst} }
#' \code{\link[lidR:getFirstLast]{getFirstLast} }
#' \code{\link[lidR:getFirstOfMany]{getFirstOfMany} }
#' \code{\link[lidR:getSingle]{getSingle} }
#' \code{\link[lidR:getLast]{getLast} }
#' \code{\link[lidR:getGround]{getGround} }
#' \code{\link[lidR:getNth]{getNth} }
#' \code{\link[lidR:leach]{leach} }
#' @export getFirstLast
#' @note \code{getFirstLast(obj)} is an alias for \code{leach(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))}
setMethod("getFirstLast", "Lidar",
	function(obj)
	{
	  ReturnNumber <- NumberOfReturns <- NULL

		return(leach(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
	}
)

#' Filter returns classified as ground
#'
#' Select only the returns classified as ground form a \code{Lidar} object
#'
#' @aliases  getGround
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
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
#' \code{\link[lidR:leach]{leach} }
#' @export getGround
#' @note \code{getGround(obj)} is an alias for \code{leach(obj, Classification == 2)}
setMethod("getGround", "Lidar",
	function(obj)
	{
	  Classification <- NULL

	 	return(leach(obj, Classification == 2))
 	}
)

#' Canopy surface model
#'
#' Creates a canopy surface model using a LIDAR point cloud.
#'
#' By default, the algorithm used is the local maximum algorithm. It assigns the
#' elevation of the highest return within each grid cell to the grid cell center.
#' It can also use a triangular irregular network (TIN) algorithm. In this case
#' it use a Delaunay triangulation on first returns. The TIN rasterization is
#' currently very slow. So, it can't be used for large datasets.
#' @aliases  canopyModel
#' @param obj An object of class \code{Lidar}
#' @param resolution numeric. The size of a grid cell in LiDAR data coordinates units
#' @param method character. The algorithm used to compute the canopy i.e. \code{"local_maxium"} or \code{"TIN"}
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' # Local maximum algorithm with a resolution of 2 meters
#' lidar %>% canopyModel(2) %>% plot
#' lidar %>% canopyModel(2) %>% plot3d
#'
#' # Local maximum and TIN algorithm on a plot with a resolution of 0.5 meters
#' forestplot = clipCircle(lidar, 685000, 5017900, 25)
#' forestplot %>% canopyModel(.5) %>% plot
#' forestplot %>% canopyModel(.5, "TIN") %>% plot
#' @seealso
#' \code{\link[lidR:gridMetrics]{gridMetrics}}
#' \code{\link[geometry:delaunayn]{delaunayn}}
#' \code{\link[lidR:rasterizeTIN]{rasterizeTIN}}
#' \code{\link[lidR:clipCircle]{clipCircle}}
#' @export canopyModel
setMethod("canopyModel", "Lidar",
	function(obj, resolution = 2, method="local_maximum")
	{
	  X <- Y <- Z <- V1 <- NULL

	  if(method == "local_maximum")
		  ret = gridMetrics(obj, resolution, max(Z)) %>% rename(Z = V1)
	  else if(method == "TIN")
	    ret = obj %>% getFirst %>% getData %$% TIN(X,Y,Z) %>% rasterizeTIN(resolution) %>% as.gridMetrics
	  else
	    stop("This algorithm does not exist")

    return(ret)
	}
)

#' Pulse density surface model
#'
#' Creates a pulse density surface model using a LIDAR point cloud.
#'
#' @aliases  pulseDensity
#' @param obj An object of class \code{Lidar}
#' @param resolution numeric. The size of a grid cell in LiDAR data coordinates units
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' lidar %>% pulseDensity(5) %>% plot
#' lidar %>% pulseDensity(5) %>% plot
#' @seealso
#' \code{\link[lidR:gridMetrics]{gridMetrics}}
#' @export pulseDensity
setMethod("pulseDensity", "Lidar",
	function(obj, resolution = 4)
	{
	  pulseID <- V1 <- NULL

		ret = gridMetrics(obj, resolution, length(unique(pulseID))/resolution^2) %>% rename(Z = V1)
    return(ret)
	}
)

#' Get LiDAR data
#'
#' Return the slot @data from an \code{Lidar} object
#'
#' @aliases  getData
#' @param obj An object of class \code{Lidar}
#' @return It returns a \code{data.table} with containing the LiDAR data
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' getData(lidar)
#' @export getData
setMethod("getData", "Lidar",
	function(obj)
	{
		return(obj@data)
	}
)

#' Clip LiDAR points within a rectangle
#'
#' Clip LiDAR points within a rectangle
#'
#' @param obj An object of class \code{Lidar}
#' @param xleft	a scalar of left x position.
#' @param ybottom	a scalar of bottom y position.
#' @param xright a scalar of right x position.
#' @param ytop a scalar of top y position.
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                     xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipRectangle
setMethod("clipRectangle", "Lidar",
	function(obj, xleft, ybottom, xright, ytop, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(leach(obj, between(X, xleft, xright), between(Y, ybottom, ytop)))
	  else
	    return(leach(obj, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

	}
)

#' Clip LiDAR points within a polygon
#'
#' Clip LiDAR points within a polygon
#'
#' @aliases clipPolygon
#' @param obj An object of class \code{Lidar}
#' @param x	a vector of x polygon's coordinates
#' @param y	a vector of y polygon's coordinates
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' subset = lidar %>% clipPolygon(x=c(685000, 685200, 685050),
#'                                  y=c(5018000, 5018100, 5018200))
#'
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipPolygon
setMethod("clipPolygon", "Lidar",
	function(obj, x, y, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(leach(obj, point.in.polygon(X,Y,x,y) > 0))
	  else
	    return(leach(obj, point.in.polygon(X,Y,x,y) == 0))
	}
)

#' Clip LiDAR points within a rectangle
#'
#' Clip LiDAR points within a rectangle
#'
#' @aliases clipRectangle
#' @param obj An object of class \code{Lidar}
#' @param xleft	a scalar of left x position.
#' @param ybottom	a scalar of bottom y position.
#' @param xright a scalar of right x position.
#' @param ytop a scalar of top y position.
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                     xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipRectangle
setMethod("clipRectangle", "Lidar",
	function(obj, xleft, ybottom, xright, ytop, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(leach(obj, between(X, xleft, xright), between(Y, ybottom, ytop)))
	  else
	    return(leach(obj, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

	}
)

#' Clip LiDAR points within a disc
#'
#' Clip LiDAR points within a disc
#'
#' @aliases clipCircle
#' @param obj An object of class \code{Lidar}
#' @param xcenter	a scalar of x circle center
#' @param ycenter	a scalar of y circle center
#' @param radius a a scalar of circle radius
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' subset = lidar %>% clipCircle(685000, 5018000, 25)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipCircle
setMethod("clipCircle", "Lidar",
	function(obj, xcenter, ycenter, radius, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(leach(obj, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
	  else
	    return(leach(obj, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
	}
)

#' Rasterize the space and compute metrics for each cells
#'
#' Computes a series of descriptive statistics for a LIDAR dataset for each cells
#' of a grid.
#'
#' Computes a series of descriptive statistics for a LIDAR data set. Output is a
#' data.frame represented in database form with each record corresponding to a
#' single grid cell. GridMetrics is similar to CloudMetrics except it computes
#' metrics for all returns  within each cell in the output grid. Cloudmetrics
#' computes a single set of metrics for the entire data set. The grid cells
#' coordinates are pre-determinted for a given resolution. So algorithm will always
#' provides the same coordinates idependently of the datset. It allows to compare
#' the same cells on several data set. Particularly the option \code{slit_fightline}
#' makes the exact same grid for each flightlines.
#'
#' @aliases  gridMetrics
#' @param obj An object of class \code{Lidar}
#' @param resolution numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @param option character. Could be \code{"slip_flightline"}. In this case algorithm will compute the metrics for each flightline individually. It return several times the same cells in overlaps.
#' @return It returns a \code{data.table} with containing the metrics for each cells. The table have the class "gridMetrics" enabling to easily plot it.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' gridMetrics(lidar, 2, max(Z)) %>% plot
#'
#' # Mean height with 400 m^2 cells
#' gridMetrics(lidar, 20, mean(Z)) %>% plot
#'
#' # Define your own metric function
#' myMetrics = function(z, i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         hmean   = mean(z),
#'         hmax    = max(z),
#'         imean   = mean(i),
#'         angle   = mean(abs(angle))
#'         )
#'
#'    return(ret)
#'  }
#'
#' metrics = gridMetrics(lidar, 20, myMetrics(Z, Intensity, ScanAngle, pulseID))
#' plot(metrics, "hmean")
#' plot(metrics, "hmax")
#' plot(metrics, "imean")
#' #etc...)
#' @export gridMetrics
#' @importFrom plyr round_any
setMethod("gridMetrics", "Lidar",
	function(obj, resolution, func, option = NULL)
	{
	  func_call = substitute(func)

		x_raster = plyr::round_any(obj@data$X, resolution)
		y_raster = plyr::round_any(obj@data$Y, resolution)
		flightlineID = obj@data$flightlineID

 		if(is.null(option))
 			by = list(Xc = x_raster,Yc = y_raster)
 		else if(option == "split_flightline")
 			by = list(Xc = x_raster,Yc = y_raster, flightline = flightlineID)
 		else
			stop("Cette option n'existe pas")

		stat <- obj@data[, c(eval(func_call)), 	by=by]

		n = names(stat)
		n[1:2] = c("X", "Y")
		setnames(stat, n)

		attr(stat, "class") = c("gridMetrics", attr(stat, "class"))

		return(stat)
	}
)

#' Compute metrics for a cloud of points
#'
#' Computes a series of descriptive statistics for a LIDAR dataset
#'
#' Computes a series of descriptive statistics for a LIDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set.#'
#' @aliases  cloudMetrics
#' @param obj An object of class \code{Lidar}
#' @param func The function to be apply to cloud of points
#' @return It returns a \code{data.table} with containing the metrics
#' @export cloudMetrics
setMethod("cloudMetrics", "Lidar",
	function(obj, func)
	{
	  if(!is.null(inargs$resolution))
	     resolution = inargs$resolution
	   else
	     resolution = 2

	  if(!is.null(inargs$threshold))
	     threshold = inargs$threshold
	   else
	     threshold = 2

	  func_call = substitute(func)

	  metric = obj@data %$% eval(func_call)

		return(metric)
	}
)

#' Thin LiDAR data
#'
#' Thin LIDAR data removing randomly a given proportion of pulses to reach a specific pulse densities
#'
#' Thin is designed to produce output data sets that have uniform pulse densities
#' throughout the coverage area. For each cell, the proportion of pulses that will
#' be retained is computed using the calculated pulse density and the desired pulse
#' density. If required pulse density is greater than the local pulse density it returns
#' an unchanged set of points (it cannot increases the pulse density). In the way
#' of \code{homogenize = FALSE} it randomly remove pulses to reach the required pulse
#' density on the whole area (see \code{\link[lidR:area]{area}}).
#' @aliases  thin
#' @param obj An object of the class \code{Lidar}
#' @param pulseDensity numeric. The pulseDensity expected
#' @param homogenize logical. If \code{TRUE}, algorithm tries to homogenize the pulse density to provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density on the whole area.
#' @param resolution numeric. Cell size to compute the pulse density.
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' # By default the method method is homogenize = TRUE
#' thinned = lidar %>% thin(1, resolution = 5)
#' lidar   %>% pulseDensity %>% plot
#' thinned %>% pulseDensity %>% plot
#'
#' # Method homogenize = FALSE enables to reach a global pulse density
#' thinned = lidar %>% thin(1, homogenize = FALSE)
#' thinned %>% summary
#' thinned %>% pulseDensity %>% plot
#' @export thin
#' @importFrom plyr round_any
setMethod("thin", c("Lidar", "numeric"),
	function(obj, pulseDensity, homogenize = TRUE, resolution = 5)
  {
	  pulseID <- gpstime <- NULL

    if(homogenize == FALSE)
    {
      n = round(pulseDensity*obj@area)
      selected = selectPulseToRemove(obj@data$pulseID, n)
    }
    else
    {
      n = round(pulseDensity*resolution^2)

      x_raster = plyr::round_any(obj@data$X, resolution)
      y_raster = plyr::round_any(obj@data$Y, resolution)

      by = list(Xr = x_raster,Yr = y_raster)

      selected = obj@data[, list(delete = selectPulseToRemove(pulseID, n), t = gpstime), by=by]
      selected[, c("Xr", "Yr") := NULL]

      setorder(selected, t)
      setorder(obj@data, gpstime)

      selected = selected$delete
    }

    return(Lidar(obj@data[selected]))
	}
)

#' Extent
#'
#' Returns an Extent object of a \code{Lidar} object.
#'
#' @aliases extent
#' @param x An object of the class \code{Lidar}
#' @param \dots Unused
#' @return Extent object
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' extent(lidar)
#' @seealso \code{\link[raster:extent]{raster::extent} }
#' @export extent
#' @importFrom raster extent
setMethod("extent", "Lidar",
	function(x)
	{
		return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
	}
)

#' Plot LiDAR data
#'
#' Plot in 3D a LiDAR cloud of point
#'
#' @aliases plot plot-LiDAR
#' @param x An object of the class \code{Lidar}
#' @param y Unused (inherited from base plot)
#' @param color characters. The field used to colorize the points. Default is Z coordinates
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param \dots supplementary parameters for \link[rgl:points3d]{points3d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' plot(lidar)
#' plot(lidar, color = "Intensity", colorPalette = "heat.colors")
#' @seealso \code{\link[rgl:points3d]{points3d} }
#' @export plot
#' @importFrom rgl points3d open3d
#' @importFrom grDevices heat.colors terrain.colors topo.colors
setMethod("plot", "Lidar",
	function(x, y, color = "Z", colorPalette = "height.colors", ...)
	{
		inargs <- list(...)

		q = ifelse(is.null(inargs$q), 1, inargs$q)

		data = unlist(x@data[,color, with = FALSE])
		inargs$col = .colorPalette(data, q, colorPalette)

		inargs$col[is.na(inargs$col)] = "lightgray"

		rgl::open3d()
		do.call(rgl::points3d, c(list(x=x@data$X, y=x@data$Y, z=x@data$Z), inargs))
	}
)

#' Summary of Lidar data
#'
#' Summary of Lidar data
#'
#' @aliases summary
#' @param object An object of the class \code{Lidar}
#' @param \dots Unused
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' summary(lidar)
#' @export summary
setMethod("summary", "Lidar",
	function(object, ...)
  {
	  size <- format(object.size(object), units = "auto")

    cat(paste("Memory :", size, "\n", sep=" "))

	  cat("\n")

    cat("area :", object@area, "square units\n")
    cat("points :", dim(object)[1], "points\n")
		cat("pulses :", n_distinct(object@data$pulseID), "pulses\n")
    cat("point density :", object@pointDensity, "points/square units\n")
    cat("pulse density :", object@pulseDensity, "pulses/square units\n")
	}

)

#' Dimensions of a Lidar object
#'
#' Retrieve the dimension of a Lidar object.
#'
#' @aliases dim
#' @param x An object of the class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' dim(lidar)
#' @export dim
setMethod("dim", "Lidar",
 	function(x)
 	{
 		return(dim(x@data))
 	}
)

#' Area of a Lidar object
#'
#' Retrieve the area of a Lidar object.
#'
#' area is computed with a convex hull. It is only an approximation if the shape of
#' the data is not convex.
#' @aliases area
#' @param obj An object of the class \code{Lidar}
#' @return numeric. The area of the object computed with a convex hull in Lidar coordinates units
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = Lidar(LASfile)
#'
#' area(lidar)
#' @seealso
#' \code{\link[lidR:convexHull]{convexHull} }
#' \code{\link[lidR:polygonArea]{polygonArea} }
#' @export area
setMethod("area", "Lidar",
	function(obj)
	{
		hull = convexHull(obj@data$X, obj@data$Y)
		area = polygonArea(hull$x, hull$y)
		area = round(area,1)
		return(area)
	}
)

#' Classify LiDAR points from the polygons in a shapefile
#'
#' Classify LiDAR points from the polygons in a ESRI shapefile
#'
#' Classify Lidar points based on geographic data found in a shapefile. It check
#' if the LiDAR points are in polygons given in the shapefile. If the parameter
#' \code{field} is the name of a field of the shapefile if classify the points
#' based on the data in the shapefile. Else it classify points as boolean. TRUE
#' if the points is in a polygon, FALSE otherwise. This function allows to filter
#' lakes for example.
#' @aliases classifyFromShapefile
#' @param obj An object of the class \code{Lidar}
#' @param shapefile An object of class SpatialPolygonsDataFrame
#' @param field characters. The name of a field of the shapefile or the name of the new field in the Lidar object.
#' @return An object of the class \code{Lidar} with a new field
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' lidar = Lidar(LASfile)
#' lakes = rgdal::readOGR(shapefile_dir, "lac_ontario_UTM17")
#'
#' # The field "lake" does not exist in the shapefile. Points are classified as TRUE if in a polygon
#' lidar = classifyFromShapefile(lidar, lakes, "inlakes")
#' forest = leach(lidar, inlakes == FALSE)
#' plot(lidar)
#' plot(forest)
#'
#' # The field "LAKENAME_1" exist in the shapefile. Points are classified with the value of the polygon
#' lidar = classifyFromShapefile(lidar, lakes, "LAKENAME_1")
#' @seealso
#' \code{\link[rgdal:readOGR]{readOGR} }
#' \code{\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame} }
#' @export classifyFromShapefile
#' @importFrom sp point.in.polygon
#' @importFrom raster crop
#' @importFrom rgdal readOGR
setMethod("classifyFromShapefile", c("Lidar"),
	function(obj, shapefile, field)
	{
		polys   = raster::crop(shapefile, extent(obj))
		npoly   = length(polys@polygons)
		npoints = dim(obj)[1]

		if(field %in% names(polys@data))
		{
		  method = 1

		  data = polys@data[,field]

		  if(class(data) == "factor")
		    values = factor(rep(NA, npoints), levels = levels(data))
		  else
		    values = numeric(npoints)
		}
		else
		{
		  method = 2
		  values = numeric(npoints)
		}

		for(i in 1:npoly)
		{
		  x = polys@polygons[[i]]@Polygons[[1]]@coords[,1]
		  y = polys@polygons[[i]]@Polygons[[1]]@coords[,2]

		  if(method == 1)
		  {
		    bool = sp::point.in.polygon(obj@data$X, obj@data$Y, x, y) > 0
		    values[bool] = data[i]
		  }
		  else if(method == 2)
		  {
		    values = values + sp::point.in.polygon(obj@data$X, obj@data$Y, x, y)
		  }
		}

		if(method == 2)
		  values = values > 0

		obj@data$info = values

		colnames = names(obj@data)
		colnames[length(colnames)] = field
    setnames(obj@data, colnames)

    return(obj)
	}
)

setMethod(".pointDensity", "Lidar",
	function(obj)
	{
		d = obj@data %>% nrow %>% divide_by(obj@area) %>% round(2)
		return(d)
	}
)

setMethod(".pulseDensity", "Lidar",
	function(obj)
	{
		d = obj@data$pulseID %>% n_distinct %>% divide_by(obj@area) %>% round(2)
		return(d)
	}
)

#  ========= EN DEVELOPPEMENT =========

# setGeneric("getDTM", function(obj, rasterArea = 4){standardGeneric("getDTM")})
# setMethod("getDTM", "Lidar",
# 	function(obj, rasterArea = 4)
# 	{
# 	  ground = obj %>% getGround
#
# 	  x = ground@data$X
#     y = ground@data$Y
#     z = ground@data$Z
#
#     dtm = .rasterize_tin(x, y, z, 100, 100)
#
# 		return(DigitalModel(dtm, 10))
# 	}
# )