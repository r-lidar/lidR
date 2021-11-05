#' Concave and convex hulls for LAS objects
#'
#' Concave and convex hulls for LAS objects. `st_convex_hull` extends `sf::st_convex_hull` for LAS
#' objects, Both functions return a `sfc_POLYGON` `concaveman` is very a fast 2D concave hull algorithm
#' for a set of points
#'
#' The concaveman algorithm is based on ideas from Park and Oh (2012). A first implementation in
#' JavaScript was proposed by Vladimir Agafonkin in \href{https://github.com/mapbox/concaveman}{mapbox}.
#' This implementation dramatically improved performance over the one stated in the paper
#' using a spatial index. The algorithm was then ported to R by Joël Gombin in the R package
#' \href{https://github.com/joelgombin/concaveman}{concaveman} that runs the JavaScript
#' implementation proposed by Vladimir Agafonkin. Later a C++ version of Vladimir Agafonkin's
#' JavaScript implementation was proposed by Stanislaw Adaszewski in
#' \href{https://github.com/sadaszewski/concaveman-cpp}{concaveman-cpp}. This concaveman
#' function uses Stanislaw Adaszewski's C++ code making the concaveman algorithm an
#' order of magnitude (up to 50 times) faster than the Javascript version.
#'
#' @param x,y An object of class LAS or XY coordinates of points in case of `concaveman`. This can be
#' specified as two vectors x and y, a 2-column matrix x, a list with two components, etc.
#' @param concavity numeric a relative measure of concavity. 1 results in a relatively detailed shape,
#' Infinity results in a convex hull. You can use values lower than 1, but they can produce pretty crazy
#' shapes.
#' @param length_threshold numeric. When a segment length is under this threshold, it stops being
#' considered for further detailed processing. Higher values result in simpler shapes.
#' @return A `sfc_POLYGON` from `sf` or a `data.frame` in case of `concaveman`
#' @importFrom sf st_convex_hull
#' @name st_hull
#' @rdname st_hull
#' @md
#' @references Park, J.-S & Oh, S.-J. (2013). A New Concave Hull Algorithm and Concaveness Measure
#' for n-dimensional Datasets. Journal of Information Science and Engineering. 29. 379-392.
#' @examples
#' x <- runif(35)
#' y <- runif(35)
#' hull <- concaveman(x,y)
#' plot(x,y, asp = 1)
#' lines(hull, lwd = 3, col = "red")
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-drop_z_below 1")
#' hull = st_concave_hull(las)
#' plot(hull)
NULL

#' @export
#' @rdname st_hull
st_concave_hull <- function(x, concavity = 2, length_threshold = 5) UseMethod("st_concave_hull")

#' @export
#' @rdname st_hull
st_convex_hull <- function(x) UseMethod("st_convex_hull")

#' @export
st_convex_hull.LAS = function(x)
{
  chull <- convex_hull(x$X, x$Y)
  chull <- sf::st_polygon(list(as.matrix(chull)))
  chull <- sf::st_geometry(chull)
  sf::st_crs(chull) <- st_crs(x)
  return(chull)
}

#' @export
st_concave_hull.LAS = function(x, concavity = 2, length_threshold = 5)
{
  chull <- concaveman(x$X, x$Y, concavity, length_threshold)
  chull <- sf::st_polygon(list(as.matrix(chull)))
  chull <- sf::st_geometry(chull)
  sf::st_crs(chull) <- st_crs(x)
  return(chull)
}

#' @rdname st_hull
#' @export
concaveman <- function(x, y = NULL, concavity = 2, length_threshold = 0)
{
  stopifnot(is.numeric(concavity), is.numeric(length_threshold), length(concavity) == 1L, length(length_threshold) == 1L)

  if (is.null(y)) {
    if (!is.matrix(x) && !is.data.frame(x) && !is.list(x)) stop("A matrix a list or a data.frame is expected.")
    if (dim(x)[2] != 2) stop("Two columns are expected.")
    if (is.matrix(x)) {
      y <- x[,2]
      x <- x[,1]
    } else {
      y <- x[[2]]
      x <- x[[1]]
    }
  }

  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), length(x) >= 3L)

  h <- grDevices::chull(x, y) - 1
  return(cpp_concaveman(x, y, concavity, length_threshold, h))
}


convex_hull = function(x, y)
{
  i <- grDevices::chull(x,y)
  i <- c(i, i[1])
  coords <- list(x = x[i], y = y[i])
  data.table::setDF(coords)
  return(coords)
}

area_convex_hull = function(x, y)
{
  stopifnot(length(x) == length(y))
  hull <- convex_hull(x, y)
  area <- polygon_area(hull$x, hull$y)
  return(area)
}

polygon_area = function(x, y)
{
  if (length(x) == 0 && length(y) == 0) return(0)
  if (!is.numeric(x) || !is.numeric(y) ) stop("Arguments 'x' and 'y' must be real")  # nocov
  if (length(x) != length(y)) stop("Argument 'x' and 'y' must be of same size") # nocov

  area <- 0
  j <- length(x)

  for (i in 1:j)
  {
    area <- area + (x[j] + x[i])*(y[j] - y[i]);
    j <- i;
  }

  area <- abs(area*0.5)
  return(area)
}
