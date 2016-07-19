#' Plot an object of class gridMetrics in 3D
#'
#' @param x A data.frame or data.table of class gridMetrics.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param \dots Other parameters for \link[rgl:surface3d]{surface3}
#' @seealso
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[rgl:surface3d]{surface3d}
#' \link[lidR:plot.gridMetrics]{plot2d}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' # Canopy surface model with 4 m^2 cells
#' gridMetrics(lidar, 2, max(Z)) %>% plot3d
#'
#' # Mean height with 400 m^2 cells
#' gridMetrics(lidar, 20, mean(Z)) %>% plot3d
#' @importFrom rgl surface3d
#' @export
plot3d = function(x, z = NULL, ...)
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

  if(is.null(inargs$col))
	{
  	zlim       <- range(mtx, na.rm = TRUE)
  	zlen       <- zlim[2] - zlim[1] + 1
  	colorlut   <- height.colors(zlen)
  	inargs$col <- colorlut[ mtx-zlim[1]+1 ]
  }

  do.call(rgl::surface3d, c(list(x=rownames(mtx), y=colnames(mtx), z=mtx), inargs))
}