#' An S4 class to represent a LiDAR dataset.
#'
#' An S4 class to represent a LiDAR dataset. It contains the data, the header and additionnal
#' values computed during the loading.
#'
#' A \code{Lidar} object contains a \code{data.table} in the slot \code{@data} with the data
#' read from a \code{.las} file and other informations computed during the data loading. The
#' fields read from the las file are named:
#' \itemize{
#' \item{\code{X Y Z}}
#' \item{\code{Intensity}}
#' \item{\code{ReturnNumber}}
#' \item{\code{NumberOfReturns}}
#' \item{\code{ScanDirectionFlag}}
#' \item{\code{EdgeofFlightline}}
#' \item{\code{Classification}}
#' \item{\code{ScanAngle}}
#' \item{\code{UserData}}
#' \item{\code{PointSourceID}}
#' }
#' When a \code{Lidar} object is built, two other informations are computed in the \code{data.table}:
#' \itemize{
#' \item{\code{pulseID}: }{a number which identifies each pulse allowing to know from which beam a point comes from}
#' \item{\code{flightlineID}: }{a number which identifies the flightline allowing to know from which flighline a point comes from}}
#' A \code{Lidar} object contains other informations in slots \code{@area}, \code{@pointDensity} and \code{@pulseDensity}:
#' \itemize{
#' \item{\code{area}: }{is computed with a convex hull. It is only an approximation if the shape of the data is not convex.}
#' \item{\code{points} and \code{pulse density}: }{are computed with the computed area. Therefore it suffers of the same issue.}
#' }
#' A \code{Lidar} object also contains a slot \code{@header} which contains the header of the \code{.las} file.
#' See public documentation of \code{.las} file format for more information.
#'
#' @slot data data.table. a table representing the LiDAR data
#' @slot area numeric. The area of the dataset computed with a convex hull
#' @slot pointDensity numeric. The point density of the dataset
#' @slot pulseDensity numeric. The pulse density of the dataset
#' @slot header list. A list of information contained is the las file header.
#' @seealso
#' \link[lidR:LoadLidar]{LoadLidar}
#' @name Lidar-class
#' @rdname Lidar-class
#' @aliases Lidar
#' @exportClass Lidar
#' @include setGeneric.r
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

#' Load a las file and create a 'Lidar' object
#'
#' Methods to read and creates a \code{Lidar} object from a vector of .las filename(s)
#'
#' Methods to read and creates a \code{Lidar} object from a vector of .las filename(s).
#' The option fields enable to select which fields will be loaded. Removing useless field
#' allows to save memory. The option '\code{minimal}' load only X,Y,Z and gpstime allowing
#' to compute pulseID and flightlineID. The option '\code{standard}' load all the fiels
#' minus UserDate, EdgeofFlighline and PointSourceID. The option '\code{all}' load
#' everinthing.
#' @param input character. Filename of .las file. Use \link[base:c]{c()} to concatain several files.
#' @param fields character. Can be \code{"minimal"}, \code{"standard"}, \code{"all"}. Default is standard. See details.
#' @param \dots Unused
#' @return An object of the class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' getData(lidar)
#' summary(lidar)
#' @seealso
#' \link[lidR:Lidar]{Class Lidar}
#' \link[lidR:getData]{getData}
#' \link[lidR:summary]{summary}
#' @export LoadLidar
LoadLidar <- function(input, fields = "standard", ...) {return(new("Lidar", input, fields, ...))}

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

#' @rdname extract
setMethod("extract", "Lidar",
	function(.data, ...)
	{
		ret = .data@data %>% dplyr::filter(...) %>% LoadLidar

		return(ret)
	}
)

#' @rdname getNth
setMethod("getNth", "Lidar",
	function(obj, n)
	{
	   ReturnNumber <- NULL

	  if(n > max(obj@data$ReturnNumber) | n <= 0)
	    stop("Parameter n of function getNth incorrect")

		return(extract(obj, ReturnNumber == n))
	}
)

#' @rdname getFirst
setMethod("getFirst", "Lidar",
	function(obj)
	{
		return(getNth(obj, 1))
	}
)
#' @rdname getFirstOfMany
setMethod("getFirstOfMany", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(extract(obj, NumberOfReturns > 1, ReturnNumber == 1))
	}
)

#' @rdname getSingle
setMethod("getSingle", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- NULL

		return(extract(obj, NumberOfReturns == 1))
	}
)

#' @rdname  getLast
setMethod("getLast", "Lidar",
	function(obj)
	{
	  NumberOfReturns <- ReturnNumber <- NULL

		return(extract(obj, ReturnNumber == NumberOfReturns))
	}
)

#' @rdname getFirstLast
setMethod("getFirstLast", "Lidar",
	function(obj)
	{
	  ReturnNumber <- NumberOfReturns <- NULL

		return(extract(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
	}
)

#' @rdname getGround
setMethod("getGround", "Lidar",
	function(obj)
	{
	  Classification <- NULL

	 	return(extract(obj, Classification == 2))
 	}
)

#' @rdname canopyModel
setMethod("canopyModel", "Lidar",
	function(obj, res = 2, method="local_maximum", start=c(0,0))
	{
	  X <- Y <- Z <- V1 <- NULL

	  if(method == "local_maximum")
		  ret = gridMetrics(obj, res, max(Z), start) %>% dplyr::rename(Z = V1)
	  else if(method == "TIN")
	    ret = obj %>% getFirst %>% getData %$% TIN(X,Y,Z) %>% rasterizeTIN(res) %>% as.gridMetrics
	  else
	    stop("This algorithm does not exist")

    return(ret)
	}
)

#' @rdname pulseDensity
setMethod("pulseDensity", "Lidar",
	function(obj, res = 4)
	{
	  pulseID <- V1 <- NULL

		ret = gridMetrics(obj, res, length(unique(pulseID))/res^2) %>% dplyr::rename(Z = V1)
    return(ret)
	}
)

#' @rdname getData
setMethod("getData", "Lidar",
	function(obj)
	{
		return(obj@data)
	}
)

#' @rdname clipRectangle
setMethod("clipRectangle", "Lidar",
	function(obj, xleft, ybottom, xright, ytop, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, between(X, xleft, xright), between(Y, ybottom, ytop)))
	  else
	    return(extract(obj, !between(X, xleft, xright), !between(Y, ybottom, ytop)))

	}
)

#' @rdname clipPolygon
setMethod("clipPolygon", "Lidar",
	function(obj, x, y, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, sp::point.in.polygon(X,Y,x,y) > 0))
	  else
	    return(extract(obj, sp::point.in.polygon(X,Y,x,y) == 0))
	}
)

#' @rdname clipCircle
setMethod("clipCircle", "Lidar",
	function(obj, xcenter, ycenter, radius, inside = TRUE)
	{
	  X <- Y <- NULL

	  if(inside)
		  return(extract(obj, (X-xcenter)^2 + (Y-ycenter)^2 <= radius^2))
	  else
	    return(extract(obj, (X-xcenter)^2 + (Y-ycenter)^2 > radius^2))
	}
)

#' @rdname gridMetrics
setMethod("gridMetrics", "Lidar",
	function(obj, res, func, start = c(0,0), option = NULL)
	{
	  func_call = substitute(func)

		x_raster = plyr::round_any(obj@data$X-0.5*res-start[1], res)+0.5*res+start[1]
		y_raster = plyr::round_any(obj@data$Y-0.5*res-start[2], res)+0.5*res+start[2]
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

#' @rdname cloudMetrics
setMethod("cloudMetrics", "Lidar",
	function(obj, func)
	{
	  func_call = substitute(func)

	  metric = obj@data %$% eval(func_call)

		return(metric)
	}
)

#' @rdname thin
setMethod("thin", c("Lidar", "numeric"),
	function(obj, pulseDensity, homogenize = TRUE, resolution = 5)
  {
	  pulseID <- gpstime <- NULL

    if(homogenize == FALSE)
    {
      n = round(pulseDensity*obj@area)
      selected = .selectPulseToRemove(obj@data$pulseID, n)
    }
    else
    {
      n = round(pulseDensity*resolution^2)

      x_raster = plyr::round_any(obj@data$X, resolution)
      y_raster = plyr::round_any(obj@data$Y, resolution)

      by = list(Xr = x_raster,Yr = y_raster)

      selected = obj@data[, list(delete = .selectPulseToRemove(pulseID, n), t = gpstime), by=by]
      selected[, c("Xr", "Yr") := NULL]

      setorder(selected, t)
      setorder(obj@data, gpstime)

      selected = selected$delete
    }

    return(LoadLidar(obj@data[selected]))
	}
)

#' @rdname extent
setMethod("extent", "Lidar",
	function(x)
	{
		return(raster::extent(min(x@data$X), max(x@data$X), min(x@data$Y), max(x@data$Y)))
	}
)

#' Plot LiDAR data
#'
#' This functions implements a 3D plot method for Lidar objects
#'
#' @aliases plot plot.Lidar
#' @param x An object of the class \code{Lidar}
#' @param y Unused (inherited from R base)
#' @param color characters. The field used to colorize the points. Default is Z coordinates
#' @param colorPalette characters. A color palette name. Default is \code{height.colors} provided by the package lidR
#' @param bg The color for the background Default is black.
#' @param \dots Supplementary parameters for \link[rgl:points3d]{points3d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' plot(lidar)
#' plot(lidar, color = "Intensity", colorPalette = "heat.colors")
#' @seealso
#' \link[rgl:points3d]{points3d}
#' \link[lidR:height.colors]{height.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[lidR:Lidar]{Class Lidar}
#' @export
#' @importFrom rgl points3d open3d rgl.bg
#' @importFrom grDevices heat.colors terrain.colors topo.colors
plot.Lidar = function(x, y, color = "Z", colorPalette = "height.colors", bg = "black",  ...)
{
  inargs <- list(...)

  q = ifelse(is.null(inargs$q), 1, inargs$q)

  data = unlist(x@data[,color, with = FALSE])
  inargs$col = .colorPalette(data, q, colorPalette)

  inargs$col[is.na(inargs$col)] = "lightgray"

  rgl::open3d()
  rgl::rgl.bg(color = bg)
  do.call(rgl::points3d, c(list(x=x@data$X, y=x@data$Y, z=x@data$Z), inargs))
}

#' Summary of Lidar data
#'
#' This functions implements a \link[base:summary]{summary} method for Lidar object
#'
#' @aliases summary
#' @param object An object of the class \code{Lidar}
#' @param \dots Unused (inherited from R base)
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' summary(lidar)
#'
#' @export
#' @seealso
#' \link[lidR:Lidar]{Class Lidar}
summary.Lidar =	function(object, ...)
{
  size <- format(object.size(object), units = "auto")

  cat(paste("Memory :", size, "\n", sep=" "))

  cat("\n")

  cat("area :", object@area, "square units\n")
  cat("points :", dim(object@data)[1], "points\n")
  cat("pulses :", dplyr::n_distinct(object@data$pulseID), "pulses\n")
  cat("point density :", object@pointDensity, "points/square units\n")
  cat("pulse density :", object@pulseDensity, "pulses/square units\n")
}

#' @rdname area
setMethod("area", "Lidar",
	function(obj)
	{
		hull = convexHull(obj@data$X, obj@data$Y)
		area = polygonArea(hull$x, hull$y)
		area = round(area,1)
		return(area)
	}
)

#' @rdname classifyFromShapefile
setMethod("classifyFromShapefile", "Lidar",
	function(obj, shapefile, field)
	{
	  npoints = dim(obj@data)[1]

		if(field %in% names(shapefile@data))
		{
		  method = 1

		  if(class(shapefile@data[,field]) == "factor")
		    values = factor(rep(NA, npoints), levels = levels(shapefile@data[,field]))
		  else
		    values = numeric(npoints)
		}
		else
		{
		  method = 2
		  values = numeric(npoints)
		}

	  polys   = raster::crop(shapefile, extent(obj))
		npoly   = length(polys@polygons)

		if(npoly > 0)
		{
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
