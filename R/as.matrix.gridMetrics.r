#' Tranform a gridMetrics object into a spatial matrix
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
#' lidar = readLAS(LASfile)
#'
#' meanHeight = gridMetrics(lidar, 20, mean(Z))
#' mtx = as.matrix(meanHeight)
#' @importFrom reshape2 acast
#' @export
#' @method as.matrix gridMetrics
#' @importFrom magrittr %>%
as.matrix.gridMetrics = function(x, z = NULL, ...)
{
  inargs <- list(...)

  multi = duplicated(x, by = c("X","Y")) %>% sum

  if(multi > 0 & is.null(inargs$fun.aggregate))
     lidRError("GDM2", number = multi, behavior = message)

  if(is.null(inargs$fun.aggregate))
    inargs$fun.aggregate = mean

  if(is.null(z))
  {
    if(length(names(x)) > 3)
      lidRError("GDM3")
    else
      z = names(x)[3]
  }

  mtx = do.call(reshape2::acast, c(list(data = x, formula = X~Y, value.var=z), inargs))

  return(mtx)
}
