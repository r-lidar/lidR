#' Canopy Closure
#'
#' canopyClosure computes estimates of canopy closure using a canopy surface model.
#' Output values for cover estimates range from 0.0 to 100.0 percent. Canopy closure
#' is defined as the number of cells over a specified height threshold divided by
#' the total cells of the canopy.
#'
#' @param x numeric matrix. A canopy surface model
#' @param threshold numeric. Threshold value
#' @param na.rm logical. If TRUE it doesn't count NA value in the number of cells (useful for circular plot for example)
#' @return A numeric percentage between 0 and 100
#' @seealso
#' \link[lidR:canopyMatrix]{canopyMatrix}
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:cloudMetrics]{cloudMetrics}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
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
#' @export canopyClosure
canopyClosure = function(x, threshold, na.rm = TRUE)
{
  if(!is.matrix(x))
    stop("'x' is not a correct type")

  num  = sum(x > threshold, na.rm=TRUE)

  if(na.rm)
    over = length(x[!is.na(x)])*100
  else
    over = length(x)*100

  CC  = num/over

  return(CC)
}