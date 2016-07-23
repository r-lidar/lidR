#' Select an region of interest interactively
#'
#' Select an region of interest contained into a rectangle using interactively the mouse
#'
#' \code{selectArea} enable the user to select a region of interest (ROI) drawing
#' a rectangle with the mouse
#' @aliases selectArea
#' @param obj An object of class \code{Lidar}
#' @param \dots Optionnal parameters for the plot function
#' @return An object of class \code{Lidar}
#' @examples
#'\dontrun{
#' LASfile <- system.file("extdata", "Megaplot.las", package="lidR")
#'
#' lidar = LoadLidar(LASfile)
#'
#' subset = selectArea(lidar)
#' }
#' @export selectArea
#' @importFrom rgl view3d select3d rgl.close
#' @importFrom magrittr %$% %>%
setGeneric("selectArea", function(obj, ...){standardGeneric("selectArea")})

#' @rdname selectArea
setMethod("selectArea", "Lidar",
  function(obj, ...)
  {
    plot.Lidar(obj, ...)
    rgl::view3d(0,0)

    f = rgl::select3d()
    keep = obj@data %$% f(X,Y,0)
    rgl::rgl.close()

    out = obj %>% extract(keep)
    plot.Lidar(out, ...)

    return(out)
  }
)
