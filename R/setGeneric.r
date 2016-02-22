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
#' lidar = LoadLidar(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = lidar %>% leach(Classification == 1, ReturnNumber == 1)
#' @seealso
#' \code{\link[dplyr:filter]{filter}}
#' @export leach
#' @importFrom dplyr filter
setGeneric("leach", function(.data, ...){standardGeneric("leach")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getNth
#' @note \code{getNth(obj, n)} is an alias for \code{leach(obj, ReturnNumber == n)}
#' @aliases  getNth
setGeneric("getNth", function(obj, n){standardGeneric("getNth")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getFirst
#' @note \code{getFirst(obj)} is an alias for \code{leach(obj, ReturnNumber == 1)}
setGeneric("getFirst", function(obj){standardGeneric("getFirst")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getFirstOfMany
#' @note \code{getFirstOfMany(obj)} is an alias for \code{leach(obj, NumberOfReturns > 1, ReturnNumber == 1))}
setGeneric("getFirstOfMany", function(obj){standardGeneric("getFirstOfMany")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getSingle
#' @note \code{getSingle(obj)} is an alias for \code{leach(obj, NumberOfReturns == 1))}
setGeneric("getSingle", function(obj){standardGeneric("getSingle")})

#' Filter last returns
#'
#' Select only the last returns form a \code{Lidar} object i.e. the last returns and the single returns
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
#' \code{\link[lidR:leach]{leach} }
#' @export getLast
#' @note \code{getLast(obj)} is an alias for \code{leach(obj, ReturnNumber == NumberOfReturns))}
setGeneric("getLast", function(obj){standardGeneric("getLast")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getFirstLast
#' @note \code{getFirstLast(obj)} is an alias for \code{leach(obj, ReturnNumber == NumberOfReturns | ReturnNumber == 1))}
setGeneric("getFirstLast", function(obj){standardGeneric("getFirstLast")})

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
#' \code{\link[lidR:leach]{leach} }
#' @export getGround
#' @note \code{getGround(obj)} is an alias for \code{leach(obj, Classification == 2)}
setGeneric("getGround", function(obj){standardGeneric("getGround")})

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
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units
#' @param method character. The algorithm used to compute the canopy i.e. \code{"local_maxium"} or \code{"TIN"}
#' @param start vector x and y coordinates for the reference raster. Default is (0,0) see \link[lidR:gridMetrics]{gridMetrics}
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
#' Creates a pulse density surface model using a LIDAR point cloud.
#'
#' @aliases  pulseDensity
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units
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
#' @aliases  getData
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
#'                                     xright=685100, ytop =5018100)
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
#' @param x	a vector of x polygon's coordinates
#' @param y	a vector of y polygon's coordinates
#' @param inside logical. Keep data inside or outside the shape
#' @return An object of class \code{Lidar}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
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
#' @importFrom sp point.in.polygon
setGeneric("clipPolygon", function(obj, x, y, inside = TRUE){standardGeneric("clipPolygon")})

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
#' When start = (0,0) and res = 20 m gridMetrics will produce the following raster centers (10,10), (10,30), (30,10) etc..
#' When start = (-10, -10) and res = 20 m gridMetrics will produce the following raster centers (0,0), (0,20), (20,0) etc..
#' In Quebec (Canada) reference is (-831600,  117980) in NAD83.
#'
#' @aliases  gridMetrics
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @param start vector x and y coordinates for the reference raster. Default is (0,0).
#' @param option character. Could be \code{"slip_flightline"}. In this case algorithm will compute the metrics for each flightline individually. It return several times the same cells in overlaps.
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
#' plot(metrics, "hmean")
#' plot(metrics, "hmax")
#' plot(metrics, "imean")
#' #etc...)
#' @export gridMetrics
#' @importFrom plyr round_any
setGeneric("gridMetrics", function(obj, res, func, start=c(0,0), option = NULL){standardGeneric("gridMetrics")})

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
setGeneric("cloudMetrics", function(obj, func){standardGeneric("cloudMetrics")})

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
#' Classify LiDAR points from the polygons in a ESRI shapefile
#'
#' Classify Lidar points based on geographic data found in a shapefile. It check
#' if the LiDAR points are in polygons given in the shapefile. If the parameter
#' \code{field} is the name of a field of the shapefile if classify the points
#' based on the data in the shapefile. Else it classify points as boolean. TRUE
#' if the points is in a polygon, FALSE otherwise. This function allows to filter
#' lakes for example.e
#' @param obj An object of the class \code{Lidar}
#' @param shapefile An object of class SpatialPolygonsDataFrame
#' @param field characters. The name of a field of the shapefile or the name of the new field in the Lidar object.
#' @return An object of the class \code{Lidar} with a new field
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' lidar = LoadLidar(LASfile)
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
setGeneric("classifyFromShapefile", function(obj, shapefile, field){standardGeneric("classifyFromShapefile")})

#' Apply a function to a set of tiles
#'
#' Apply a function to a set of tiles using several cores
#'
#' @aliases processParallel
#' @param x  A Catalog object
#' @param func A function which have one parameter: the name of a .las file
#' @param mc.cores numeric. Number of core used. Default is "auto"
#' @param combine character. The function used to merge the outputs of the \code{func} function
#' @param \dots Other parameters for \code{mclapply}
#' @seealso
#' \link[parallel:mclapply]{mclapply}
#' @export processParallel
#' @importFrom parallel mclapply detectCores
setGeneric("processParallel", function(x, func, mc.cores = "auto", combine = "rbind", ...){standardGeneric("processParallel")})

#' Find the tiles containing plots
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
