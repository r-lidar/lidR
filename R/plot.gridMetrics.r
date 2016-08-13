#' Plot an object of class gridMetrics in 2D
#'
#' This functions implements a \link[graphics:plot]{plot} method for a gridMetrics data.frame
#'
#' The \dots param provides additional arguments to \link[fields:image.plot]{image.plot}.
#'
#' @param x A data.frame or data.table of class gridMetrics.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param colorPalette function. A color palette function. Default is \code{height.colors} provided by the package lidR
#' @param \dots Supplementary parameters for \link[fields:image.plot]{image.plot}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
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
#' @seealso
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[fields:image.plot]{image.plot}
#' \link[lidR:height.colors]{height.colors}
#' \link[lidR:forest.colors]{forest.colors}
#' \link[grDevices:heat.colors]{heat.colors}
#' \link[grDevices:colorRamp]{colorRampPalette}
#' \link[lidR:plot3d]{plot3d}
#' @importFrom fields image.plot
#' @export
#' @rdname plot.gridMetrics
#' @method plot gridMetrics
#' @importFrom magrittr %>%
plot.gridMetrics = function(x, z = NULL, colorPalette = height.colors, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM1")
    else
      z = names(x)[3]
  }

  mtx = as.matrix(x, z)
  X = rownames(mtx) %>% as.numeric
  Y = colnames(mtx) %>% as.numeric

  if(is.null(inargs$col))
    inargs$col = colorPalette(50)

  if(is.null(inargs$xlab))
    inargs$xlab = "X"

  if(is.null(inargs$ylab))
    inargs$ylab = "Y"

  if(is.null(inargs$asp))
    inargs$asp = 1

  do.call(fields::image.plot, c(list(x = X, y = Y, z = mtx), inargs))
}
