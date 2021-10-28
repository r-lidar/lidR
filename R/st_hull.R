#' Concave and convex hulls for LAS objects
#'
#' Concave and convex hulls for LAS objects. `st_convex_hull` extends `sf::st_convex_hull` for LAS
#' objects, Both functions return a polygon geometry.
#'
#' @param x An object of class LAS.
#' @param concavity,length_threshold see \link{concaveman}.
#'
#' @return A sfc_POLYGON from sf
#' @importFrom sf st_convex_hull
#' @name st_convex_hull
#' @rdname st_hull
#' @export
#' @md
NULL

#' @export
#' @rdname st_hull
st_concave_hull <- function(x, concavity = 2, length_threshold = 5) UseMethod("st_concave_hull")

#' @export
#' @rdname st_hull
st_convex_hull.LAS = function(x)
{
  chull <- convex_hull(x$X, x$Y)
  chull <- sf::st_polygon(list(as.matrix(chull)))
  chull <- sf::st_geometry(chull)
  sf::st_crs(chull) <- st_crs(x)
  return(chull)
}

#' @export
#' @rdname st_hull
st_concave_hull.LAS = function(x, concavity = 2, length_threshold = 5)
{
  chull <- concaveman(x$X, x$Y, concavity, length_threshold)
  chull <- sf::st_polygon(list(as.matrix(chull)))
  chull <- sf::st_geometry(chull)
  sf::st_crs(chull) <- st_crs(x)
  return(chull)
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
