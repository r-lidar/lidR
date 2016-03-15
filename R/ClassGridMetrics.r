#' Plot an object of class gridMetrics in 2D
#'
#' This functions implements a \link[graphics:plot]{plot} method for a gridMetrics data.frame
#'
#' The \dots param enables to provide more arguments to \link[fields:image.plot]{image.plot}.
#'
#' @param x A data.frame or data.table of class gridMetrics.
#' @param z character. The field to plot. If NULL, autodetect.
#' @param \dots Unused
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
#' @seealso
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[fields:image.plot]{image.plot}
#' \link[lidR:plot3d]{plot3d}
#' @importFrom fields image.plot
#' @export
#' @rdname plot.gridMetrics
#' @method plot gridMetrics
plot.gridMetrics = function(x, z = NULL, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      stop("More than 3 columns: please input the column's name to plot", call.=F)
    else
      z = names(x)[3]
  }

  mtx = as.matrix(x, z)
  X = rownames(mtx) %>% as.numeric
  Y = colnames(mtx) %>% as.numeric

  if(is.null(inargs$col))
    inargs$col = height.colors(100)

  if(is.null(inargs$xlab))
    inargs$xlab = "X"

  if(is.null(inargs$ylab))
    inargs$ylab = "Y"

  if(is.null(inargs$asp))
    inargs$asp = 1

  do.call(fields::image.plot, c(list(x = X, y = Y, z = mtx), inargs))
}

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
#' @export plot3d
#' @rdname plot3d
plot3d = function(x, z = NULL, ...)
{
  inargs = list(...)

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      stop("More than 3 columns: please input the column's name to plot", call.= F)
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

#' Tranform a gridMetrics into a spatial matrix
#'
#' @param x a gridMetrics object
#' @param z character. The field to plot. If NULL, autodetect
#' @param \dots Other parameters for \code{acast}
#' @seealso
#' \link[lidR:gridMetrics]{gridMetrics}
#' \link[lidR:canopyModel]{canopyModel}
#' \link[reshape2:acast]{acast}
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#' lidar = LoadLidar(LASfile)
#'
#' meanHeight = gridMetrics(lidar, 20, mean(Z))
#' mtx = as.matrix(meanHeight)
#' @importFrom reshape2 acast
#' @export
#' @rdname as.matrix
#' @method as.matrix gridMetrics
as.matrix.gridMetrics = function(x, z = NULL, ...)
{
  inargs <- list(...)

  multi = duplicated(x, by = c("X","Y")) %>% sum

  if(multi > 0 & is.null(inargs$fun.aggregate))
    message(paste(multi, "duplicated ratsers have been found. X,Y variables do not identify a single observation for each output cell. Automatic aggregation have been done using mean function"))

  if(is.null(inargs$fun.aggregate))
    inargs$fun.aggregate = mean

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      stop("More than 3 columns: please input the column's name to plot")
    else
      z = names(x)[3]
  }

  mtx = do.call(reshape2::acast, c(list(data = x, formula = X~Y, value.var=z), inargs))

  return(mtx)
}

#' Set the class gridMetrics to a data.frame or a data.table
#'
#' Set the class gridMetrics to a data.frame. Usefull when reading data from a file.
#' In this case the data.frame does not have the class gridMetrics and cannot easly be
#' plotted or transformed into a matrix.
#'
#' @param x A data.frame
#' @importFrom reshape2 acast
#' @export as.gridMetrics
as.gridMetrics = function(x)
{
  if(is.data.frame(x))
  {
    x = as.data.table(x)
    attr(x, "class") = c("gridMetrics", attr(x, "class"))
  }

  return(x)
}