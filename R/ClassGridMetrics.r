#' Plot an object of class gridMetrics in 2D
#'
#' @param x an object of class gridMetrics
#' @param z character. The field to plot. If NULL, autodetect
#' @param \dots Other parameters for \code{image.plot}
#' @seealso
#' \link[fields:image.plot]{image.plot}
#' @importFrom fields image.plot
#' @export plot.gridMetrics
#' @rdname plot
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

#' Plot3d generic function
#'
#' @export plot3d
plot3d = function(x, z = NULL, ...)
{
  UseMethod("plot3d", x)
}

#' Plot an object of class gridMetrics in 3D
#'
#' @param x an object of class gridMetrics
#' @param z character. The field to plot. If NULL, autodetect
#' @param \dots Other parameters for \code{surface3d}
#' @seealso
#' \link[rgl:surface3d]{surface3d}
#' @importFrom rgl surface3d
#' @export plot3d.gridMetrics
#' @rdname plot3d
#' @method plot3d gridMetrics
plot3d.gridMetrics = function(x, z = NULL, ...)
{
  inargs <- list(...)

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
#' @seealso \link[reshape2:acast]{acast}
#' @importFrom reshape2 acast
#' @export as.matrix.gridMetrics
#' @rdname as.matrix
#' @method as.matrix gridMetrics
as.matrix.gridMetrics = function(x, z = NULL, ...)
{
  if(is.null(z))
  {
    if(length(names(x)) > 3)
      stop("More than 3 columns: please input the column's name to plot")
    else
      z = names(x)[3]
  }

  mtx = reshape2::acast(x, X~Y, value.var=z, ...)

  return(mtx)
}

#' Set the class gridMetrics to a data.frame
#'
#' Set the class gridMetrics to a data.frame. Usefull when reading data from a file.
#' In this case the data.frame does not have the class gridMetrics and cannot easly be
#' plotted or transformed into a matrix
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