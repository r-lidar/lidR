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
#' Select only the returns which return only one point.
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
#' currently very slow. Therefore, it is not suitable for large datasets.
#' @aliases  canopyModel
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of a grid cell in LiDAR data coordinates units. Default is 2 units i.e. 4 square units cells.
#' @param method character. The algorithm used to compute the canopy i.e. \code{"local_maxium"} or \code{"TIN"}
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) see \link[lidR:gridMetrics]{gridMetrics}
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which enables easier plotting.
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
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which which enables easier plotting.
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
#' Return the slot @data from a \code{Lidar} object
#'
#' @aliases getData
#' @param obj An object of class \code{Lidar}
#' @return It returns a \code{data.table} containing the LiDAR data
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

#' Rasterize the space and compute metrics for each cell
#'
#' Computes a series of descriptive statistics for a LiDAR dataset for each cell
#' of a grid.
#'
#' Computes a series of descriptive statistics defined by the user. Output is a
#' data.frame in which each line is a raster (single grid cell), each column is a metric.
#' gridMetrics is similar to cloudMetrics except it computes metrics within each cell
#' in the output grid. The grid cell coordinates are pre-determined for a given resolution.
#' So the algorithm will always provide the same coordinates independently of the dataset.
#' When start = (0,0) and res = 20 gridMetrics will produce the following raster centers: (10,10), (10,30), (30,10) etc..
#' When start = (-10, -10) and res = 20 gridMetrics will produce the following raster centers: (0,0), (0,20), (20,0) etc..
#' In Quebec (Canada) reference is (-831600,  117980) in the NAD83 coordinate system. The function to be applied to each cell is a classical function (see examples) that returns a labelled list of metrics.
#' The following existing function can help the user to compute some metrics:
#' 
#' \itemize{
#' \item{\link[lidR:entropy]{entropy}}
#' \item{\link[lidR:vci]{vci}}
#' \item{\link[lidR:canopyMatrix]{canopyMatrix}}
#' \item{\link[lidR:LAD]{LAD}}
#' \item{\link[lidR:canopyClosure]{canopyClosure}}
#' \item{\link[lidR:fractal.dimension]{fractal.dimension}}
#' \item{\link[lidR:LAD]{LAD}}
#' } Basically there are no predifined metrics. Users must write their own functions to create metrics.
#' gridMetrics will dispach the LiDAR data for each cell in the user's function. The user writes their
#' function without considering grid cells, only a cloud of points (see example).
#'
#' @aliases  gridMetrics
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @param start vector x and y coordinates for the reference raster. Default is (0,0).
#' @param option character. Could be \code{"split_flightline"}. In this case the algorithm will compute the metrics for each flightline individually. It returns the same cells several times in overlap.
#' @return It returns a \code{data.table} containing the metrics for each cell. The table has the class "gridMetrics" enabling easy plotting.
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

#' Voxelize the space and compute metrics for each voxel
#'
#' Voxelize the cloud of points and compute a series of descriptive statistics for
#' each voxel.
#'
#' Voxelize creates a 3D matrix of voxels with a given resolution. It creates a voxel
#' from the cloud of point if there is at least one point in the voxel. For each voxel
#' the function allows computation of one or several derived metrics in the same way as
#' the gridMetrics functions.
#' Basically there are no predifined metrics. Users must write their own function to create metrics.
#' Voxelize will dispach the LiDAR data for each voxel in the user's function. The user writes their
#' function without considering grid cells, only a cloud of points (see example).
#'
#' @aliases  voxelize
#' @param obj An object of class \code{Lidar}
#' @param res numeric. The size of the cells
#' @param func the function to be apply to each cells
#' @return It returns a \code{data.table} containing the metrics for each voxel. The table has the class "voxels" enabling to easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' # Cloud of points is voxelized with a 1 meter resolution and in each voxel
#' # the number of points is computed.
#' voxelize(lidar, 1, length(Z))
#'
#' # Cloud of points is voxelized with a 1 meter resolution and in each voxel
#' # the mean scan angle of points is computed.
#' voxelize(lidar, 1, mean(ScanAngle))
#'
#' # Define your own metric function
#' myMetrics = function(i, angle, pulseID)
#' {
#'   ret = list(
#'         npulse  = length(unique(pulseID)),
#'         angle   = mean(angle),
#'         imean   = mean(i)
#'         )
#'
#'    return(ret)
#'  }
#'
#' voxels = voxelize(lidar, 20, myMetrics(Intensity, ScanAngle, pulseID))
#'
#' plot(voxels, "angle")
#' plot(voxels, "imean")
#' #etc.
#' @export voxelize
#' @importFrom plyr round_any
setGeneric("voxelize", function(obj, res, func){standardGeneric("voxelize")})

#' Compute metrics for a cloud of points
#'
#' Computes a series of descriptive statistics for a LiDAR dataset
#'
#' Computes a series of descriptive statistics for a LiDAR data set. Cloudmetrics
#' computes a single set of metrics for the entire data set. See \link[lidR:gridMetrics]{gridMetrics}
#' to compute metrics on a grid. Basically there are no predifined metrics. Users
#' must write their own function to create metrics (see example). The following existing
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
#' @param func The function to be applied to a cloud of points
#' @return It returns a \code{data.table} containing the metrics
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
#' Thin LIDAR data randomly removes a given proportion of pulses to reach specific pulse densities
#'
#' Thin is designed to produce output data sets that have uniform pulse densities
#' throughout the coverage area. For each cell, the proportion of pulses that will
#' be retained is computed using the calculated pulse density and the desired pulse
#' density. If required pulse density is greater than the local pulse density it returns
#' an unchanged set of points (it cannot increase the pulse density). In the way
#' of \code{homogenize = FALSE} it randomly removes pulses to reach the required pulse
#' density on the whole area (see \code{\link[lidR:area]{area}}). The cell size must be large enough
#' to compute a coherant local pulse density. 25 square meters looks good. 1 square
#' meter is meaningless.
#' @aliases  thin
#' @param obj An object of the class \code{Lidar}
#' @param pulseDensity numeric. The expected pulseDensity
#' @param homogenize logical. If \code{TRUE}, the algorithm tries to homogenize the pulse density to provide a uniform dataset. If \code{FALSE} the algorithm will reach the pulse density on the whole area.
#' @param resolution numeric. Cell size to compute the pulse density.
#' @return It returns a \code{data.table} with the class \code{gridMetrics} which enables easier plotting.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' # By default the method is homogenize = TRUE
#' thinned = lidar %>% thin(1, resolution = 5)
#' lidar   %>% pulseDensity %>% plot
#' thinned %>% pulseDensity %>% plot
#'
#' # Method homogenize = FALSE enables a global pulse density to be reached
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
#' Classify Lidar points based on geographic data found in a shapefile. It checks
#' if the LiDAR points are in polygons given in the shapefile. If the parameter
#' \code{field} is the name of a field in the shapefile it classifies the points
#' based on the data in the shapefile. Else it classifies the points as boolean. TRUE
#' if the points are in a polygon, FALSE otherwise. This function allows for filtering
#' lakes, for example.
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
#' # The field "LAKENAME_1" exists in the shapefile. Points are classified with the value of the polygon
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
#' When users have a set of Lidar data organized in several tiles it can apply a user function to each tile.
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
#'   lidar = LoadLidar(LASFile)
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
setGeneric("retrieveInventoryTiles", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("retrieveInventoryTiles")})

#' Extract inventory from a set of tiles
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plot automatically.
#'
#' When the user has a set of coordinates of ground inventory, they can extract the
#' lidar data associated with these plot automatically. The algorithm automatically extracts
#' plots falling on the edges of tiles.
#'
#' @aliases extractGroundInventory
#' @param obj A Catalog object
#' @param plotnames vector. A set of plot names
#' @param x vector. A set of x plot coordinates
#' @param y vector. A set of y plot coordinates
#' @param radius numeric or vector. A radius or a set of radii of plots
#' @param buffer numeric. A buffer value to extend the search range
#' @return A list of Lidar objects
#' @export extractGroundInventory
#' @importFrom dplyr group_by summarise ungroup progress_estimated
setGeneric("extractGroundInventory", function(obj, plotnames, x, y, radius, buffer = 2){standardGeneric("extractGroundInventory")})
