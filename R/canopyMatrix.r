#' Local canopy surface model
#'
#' canopyMatrix is a reimplementation of canopyModel with the local maxium algorithm
#' for local analyse (e.g. in a raster) avoiding sides effect of the algorithm used to
#' procude the grid. It is usefull to write your own metric in a fuction.
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param z vector of z coordinates
#' @param resolution numeric. The size of the cells
#' @return A matrix which is the raster of the canopy surface model
#' @export canopyMatrix
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = Lidar(LASfile)
#'
#' myClosureMetrics = function(x, y, z)
#' {
#'    canopy = canopyMatrix(x,y,z, 2)
#'
#'    CC2  = canopyClosure(canopy, 2)
#'    CC10 = canopyClosure(canopy, 10)
#'    CC20 = canopyClosure(canopy, 20)
#'
#'    return(list(CC2 = CC2, CC10 = CC10, CC20 = CC20))
#' }
#'
#' closures = gridMetrics(lidar, 20, myClosureMetrics(X,Y,Z))
#' plot(closures, "CC2")
#' plot(closures, "CC10")
#' plot(closures, "CC20")
#' @seealso
#' \link[lidR:canopyClosure]{canopyClosure}
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:cloudMetrics]{cloudMetrics}
#' @importFrom plyr round_any
#' @importFrom reshape2 acast
canopyMatrix = function(x,y,z, resolution)
{
  X <- Y <- Z <- NULL

  xr = x - min(x)
  xr[xr == 0] = 0.001
  xr = plyr::round_any(xr, resolution, ceiling)

  yr = y - min(y)
  yr[yr == 0] = 0.001
  yr = plyr::round_any(yr, resolution, ceiling)

  frst = data.table(Z = z)

  canopy = frst[, list(Z = max(Z)), by = list(X = xr,Y = yr)]

  return(acast(canopy, X~Y, value.var = "Z"))
}
