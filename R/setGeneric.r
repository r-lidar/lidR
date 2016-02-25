#' Return points with matching conditions
#'
#' Return points with matching conditions. \code{extract} is an overloading
#' function for \code{Lidar} objects which replace the function
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
setGeneric("extract", function(.data, ...){standardGeneric("extract")})

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

#' Filter first returns
#'
#' Select only the first returns.
#'
#' @aliases  getFirst
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
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
#' \code{\link[lidR:extract]{extract} }
#' @export getFirst
#' @note \code{getFirst(obj)} is an alias for \code{extract(obj, ReturnNumber == 1)}
setGeneric("getFirst", function(obj){standardGeneric("getFirst")})

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

#' Filter single returns
#'
#' Select only the returns which return only one points.
#'
#' @aliases  getSingle
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
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
#' \code{\link[lidR:extract]{extract} }
#' @export getSingle
#' @note \code{getSingle(obj)} is an alias for \code{extract(obj, NumberOfReturns == 1))}
setGeneric("getSingle", function(obj){standardGeneric("getSingle")})

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

#' Filter first and last returns
#'
#' Select only the first and last returns.
#'
#' @aliases  getFirstLast
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
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
#' \code{\link[lidR:extract]{extract} }
#' @export getFirstLast
#' @note \code{getFirstLast(obj)} is an alias for \code{extract(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))}
setGeneric("getFirstLast", function(obj){standardGeneric("getFirstLast")})

#' Filter returns classified as ground
#'
#' Select only the returns classified as ground.
#'
#' @aliases  getGround
#' @param obj An object of class \code{Lidar}
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
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

#' Canopy surface model
#'
#' Creates a canopy surface model using a LiDAR cloud of points.
#'
#' By default, the algorithm used is the local maximum algorithm. It assigns the
#' elevation of the highest return within each grid cell to the grid cell center.
#' It can also use a triangular irregular network (TIN) algorithm. In this case
#' it use a Delaunay triangulation on first returns. The TIN rasterization is
#' currently very slow. So, it can't be used for large datasets.
#' @aliases  canopyModel
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 2 units i.e. 4 square units cells.
#' @param method character. The algorithm used to compute the canopy i.e. \code{"local_maxium"} or \code{"TIN"}
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) see \link[lidR:gridMetrics]{gridMetrics}
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
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
#' @importFrom dplyr rename
setGeneric("canopyModel", function(obj, res = 2, method="local_maximum", start = c(0,0)){standardGeneric("canopyModel")})

#' Pulse density surface model
#'
#' Creates a pulse density surface model using a LiDAR cloud of points.
#'
#' @aliases pulseDensity
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 4 units i.e. 16 square units cells.
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' lidar %>% pulseDensity(5) %>% plot
#' lidar %>% pulseDensity(5) %>% plot
#' @seealso
#' \code{\link[lidR:gridMetrics]{gridMetrics}}
#' @export pulseDensity
#' @importFrom dplyr rename
setGeneric("pulseDensity", function(obj, res = 4){standardGeneric("pulseDensity")})

#' Get LiDAR data
#'
#' Return the slot @data from an \code{Lidar} object
#'
#' @aliases getData
#' @param obj An object of class \code{Lidar}
#' @return It returns a \code{data.table} with containing the LiDAR data
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' getData(lidar)
#' @export getData
setGeneric("getData", function(obj){standardGeneric("getData")})

#' Clip LiDAR points within a rectangle
#'
#' Clip LiDAR points within a rectangle
#'
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
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipRectangle(xleft=685000, ybottom=5018000,
#'                                  xright=685100, ytop =5018100)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipRectangle
setGeneric("clipRectangle", function(obj, xleft, ybottom, xright, ytop, inside = TRUE){standardGeneric("clipRectangle")})

#' Clip LiDAR points within a polygon
#'
#' Clip LiDAR points within a polygon
#'
#' @aliases clipPolygon
#' @param obj An object of class \code{Lidar}
#' @param x	numerical array of x-coordinates of polygon
#' @param y	numerical array of y-coordinates of polygon
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipPolygon(x=c(685000, 685200, 685050),
#'                                y=c(5018000, 5018100, 5018200))
#'
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipPolygon
#' @importFrom sp point.in.polygon
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})

#' Clip LiDAR points within a disc
#'
#' Clip LiDAR points within a disc
#'
#' @aliases clipCircle
#' @param obj An object of class \code{Lidar}
#' @param xcenter	a scalar. x disc center
#' @param ycenter	a scalar. y disc center
#' @param radius a scalar. Disc radius
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = lidar %>% clipCircle(685000, 5018000, 25)
#'
#' plot(subset)
#' @seealso
#' \link[lidR:clipRectangle]{clipRectangle}
#' \link[lidR:clipCircle]{clipCircle}
#' \link[lidR:clipCircle]{clipPolygon}
#' @export clipCircle
setGeneric("clipCircle", function(obj, xcenter, ycenter, radius, inside = TRUE){standardGeneric("clipCircle")})

#' Rasterize the space and compute metrics for each cells
#'
#' Computes a series of descriptive statistics for a LiDAR dataset for each cells
#' of a grid.
#'
#' Computes a series of descriptive statistics defined by the user. Output is a
#' data.frame in which each line is a raster (single grid cell), each column is a metric.
#' gridMetrics is similar to cloudMetrics except it computes metrics within each cell
#' in the output grid. The grid cells coordinates are pre-determinted for a given resolution.
#' So the algorithm will always provides the same coordinates independently of the dataset.
#' When start = (0,0) and res = 20 gridMetrics will produce the following raster centers (10,10), (10,30), (30,10) etc..
#' When start = (-10, -10) and res = 20 gridMetrics will produce the following raster centers (0,0), (0,20), (20,0) etc..
#' In Quebec (Canada) reference is (-831600,  117980) in NAD83 coordinate system. The function to be apply to each cells
#' is a classical function (see examples) which return a labelled list of metrics. The following existing function
#' can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:vci]{vci}}
#' \item{\link[lidR:canopyMatrix]{canopyMatrix}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:canopyClosure]{canopyClosure}}
#' \item{\link[lidR:fractal.dimension]{fractal.dimension}}
#' \item{\link[lidR:LAD]{LAD}}
#' } Basically there no predifined metrics. The users must write is own function to create metrics.
#' gridMetrics will dispach the LiDAR data for each cell in the user's function. The user write his
#' function without thinking about grid cells. Just thinking about a cloud of points (see example).
#'
#' @aliases  gridMetrics
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @param start vector x and y coordinates for the reference raster. Default is (0,0).
#' @param option character. Could be \code{"split_flightline"}. In this case algorithm will compute the metrics for each flightline individually. It return several times the same cells in overlaps.
#' @return It returns a \code{data.table} with containing the metrics for each cells. The table have the class "gridMetrics" enabling to easily plot it.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
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
#'
#' plot(metrics, "hmean")
#' plot(metrics, "hmax")
#' plot(metrics, "imean")
#' #etc.
#' @export gridMetrics
#' @importFrom plyr round_any
setGeneric("gridMetrics", function(obj, res, func, start=c(0,0), option = NULL){standardGeneric("gridMetrics")})

#' Compute metrics for a cloud of points
#'
#' Computes a series of descriptive statistics for a LiDAR dataset
#'
#' Computes a series of descriptive statistics for a LiDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set. See \link[lidR:gridMetrics]{gridMetrics}
#' to compute metrics on a grid. Basically there no predifined metrics. The users
#' must write is own function to create metrics (see example). The following existing
#' function can help the user to compute some metrics:
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:vci]{vci}}
#' \item{\link[lidR:canopyMatrix]{canopyMatrix}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:canopyClosure]{canopyClosure}}
#' \item{\link[lidR:fractal.dimension]{fractal.dimension}}
#' \item{\link[lidR:LAD]{LAD}}
#' }
#' @aliases cloudMetrics
#' @param obj An object of class \code{Lidar}
#' @param func The function to be apply to cloud of points
#' @return It returns a \code{data.table} with containing the metrics
#' @export cloudMetrics
#' @seealso \link[lidR:gridMetrics]{gridMetrics}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' cloudMetrics(lidar, max(Z))
#' cloudMetrics(lidar, mean(Z))
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
#' metrics = cloudMetrics(lidar, myMetrics(Z, Intensity, ScanAngle, pulseID))
setGeneric("cloudMetrics", function(obj, func){standardGeneric("cloudMetrics")})

#' Thin LiDAR data
#'
#' Thin LIDAR data removing randomly a given proportion of pulses to reach a specific pulse densities
#'
#' Thin is designed to produce output data sets that have unifrom pulse densities
#' throughout the coverage area. For each cell, the proportion of pulses that will
#' be retained is computed using the calculated pulse density and the desired pulse
#' density. If required pulse density is greater than the local pulse density it returns
#' an unchanged set of points (it cannot increases the pulse density). In the way
#' of \code{homogenize = FALSE} it randomly remove pulses to reach the required pulse
#' density on the whole area (see \code{\link[lidR:area]{area}}). The cell size must large enought
#' to compute a coherant local pulse density. 25 square meters looks good. 1 square
#' meter does not have a meaning.
#' @aliases  thin
#' @param obj An object of the class \code{Lidar}
#' @param pulseDensity numeric. The pulseDensity expected
#' @param homogenize logical. If \code{TRUE}, algorithm tries to homogenize the pulse density to provide a unifrom dataset. If \code{FALSE} the algorithm will reach the pulse density on the whole area.
#' @param resolution numeric. Cell size to compute the pulse density.
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which anable to plot it easily.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
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
#' @importFrom dplyr n_distinct
setGeneric("thin", function(obj, pulseDensity, homogenize = TRUE, resolution = 5){standardGeneric("thin")})

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
#' lidar = LoadLidar(LASfile)
#'
#' area(lidar)
#' @seealso
#' \code{\link[lidR:convexHull]{convexHull} }
#' \code{\link[lidR:polygonArea]{polygonArea} }
#' @export area
setGeneric("area", function(obj){standardGeneric("area")})

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
#' lidar = LoadLidar(LASfile)
#'
#' extent(lidar)
#' @seealso \code{\link[raster:extent]{raster::extent} }
#' @export extent
#' @importFrom raster extent
setGeneric("extent", function(x){standardGeneric("extent")})

setGeneric(".pointDensity", function(obj){standardGeneric(".pointDensity")})
setGeneric(".pulseDensity", function(obj){standardGeneric(".pulseDensity")})

#' Classify LiDAR points from the polygons in a shapefile
#'
#' Classify LiDAR points from the polygons in an ESRI shapefile
#'
#' Classify Lidar points based on geographic data found in an shapefile. It check
#' if the LiDAR points are in polygons given in the shapefile. If the parameter
#' \code{field} is the name of a field of the shapefile if classify the points
#' based on the data in the shapefile. Else it classify points as boolean. TRUE
#' if the points is in a polygon, FALSE otherwise. This function allows to filter
#' lakes for example.
#' @param obj An object of the class \code{Lidar}
#' @param shapefile An object of class SpatialPolygonsDataFrame
#' @param field characters. The name of a field of the shapefile or the name of the new field in the Lidar object.
#' @return An object of the class \code{Lidar} with a new field
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' lidar = LoadLidar(LASfile)
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' # The field "inlake" does not exist in the shapefile. Points are classified as TRUE if in a polygon
#' lidar = classifyFromShapefile(lidar, lakes, "inlakes")
#' forest = extract(lidar, inlakes == FALSE)
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
setGeneric("classifyFromShapefile", function(obj, shapefile, field){standardGeneric("classifyFromShapefile")})

#' Apply a function to a set of tiles
#'
#' Apply a function to a set of tiles using several cores (Linux only, Windows users can only use one core, sorry...)
#'
#' When user have a set of Lidar data organized in several tiles it can apply a user function to each tiles.
#' This function describe the procedure to apply to each file beginning by the data loading (see example).
#' @aliases processParallel
#' @param x  A Catalog object
#' @param func A function which have one parameter: the name of a .las file
#' @param mc.cores numeric. Number of core used. Default is "auto"
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
#' # 3 build the function which analyse a tile (a file).
#' # This function input is only the path of a las file
#' # see the following template
#'
#' analyse_tile = function(LASFile)
#' {
#'   # Load the data
#'   lidar = LoadLidar(LASFile)
#'
#'   # Associate geographic data to lidar points (if needed)
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
#' # 5. Process the project. By default it detects how many core you have. But you can add
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

#' Retrieve the tiles containing plots
#'
#' When the user have a set of coordinates of ground inventory, he can extract the
#' lidar data associated with these plot automatically. This function retrieve the tiles
#' for each plot. This function is used by \link[lidR:extractGroundInventory]{extractGroundInventory}.
#' Users do not rally need it.
#'
#' When the user have a set of coordinates of ground inventory, he can extract the
#' lidar data associated with these plot automatically. The algorithm is able to find tiles
#' for plots falling between several tiles (on the edges) if tiles are organized in a grid.
#'
#' @aliases retrieveInventoryTiles
#' @param obj A Catalog object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radiuses of plots
#' @param buffer numeric. A buffer value to expend the extent of search
#' @export retrieveInventoryTiles
#' @importFrom dplyr mutate progress_estimated
setGeneric("retrieveInventoryTiles", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("retrieveInventoryTiles")})

#' Extract inventory from a set of tiles
#'
#' When the user have a set of coordinates of ground inventory, he can extract the
#' lidar data associated with these plot automatically.
#'
#' When the user have a set of coordinates of ground inventory, he can extract the
#' lidar data associated with these plot automatically. The algorithm automatically extract
#' plots falling on the edges of tiles.
#'
#' @aliases extractGroundInventory
#' @param obj A Catalog object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radiuses of plots
#' @param buffer numeric. A buffer value to expend the extent of search
#' @return A list of Lidar objects
#' @export extractGroundInventory
#' @importFrom dplyr group_by summarise ungroup progress_estimated
setGeneric("extractGroundInventory", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("extractGroundInventory")})
