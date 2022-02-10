#' Concave and convex hulls for LAS objects
#'
#' Concave and convex hulls for LAS objects. `st_convex_hull` extends `sf::st_convex_hull` for LAS
#' objects. Both functions return a `sfc_POLYGON`. `concaveman` is very a fast 2D concave hull algorithm
#' for a set of points.
#'
#' The concaveman algorithm is based on ideas from Park and Oh (2012). A first implementation in
#' JavaScript was proposed by Vladimir Agafonkin in \href{https://github.com/mapbox/concaveman}{mapbox}.
#' This implementation dramatically improved performance over the one stated in the paper
#' using a spatial index. The algorithm was then ported to R by Joël Gombin in the R package
#' \href{https://github.com/joelgombin/concaveman}{concaveman} that runs the JavaScript
#' implementation proposed by Vladimir Agafonkin. Later, a C++ version of Vladimir Agafonkin's
#' JavaScript implementation was proposed by Stanislaw Adaszewski in
#' \href{https://github.com/sadaszewski/concaveman-cpp}{concaveman-cpp}. This concaveman
#' function uses Stanislaw Adaszewski's C++ code making the concaveman algorithm an
#' order of magnitude (up to 50 times) faster than the Javascript version.
#'
#' @param x,y An object of class LAS or XY coordinates of points in case of `concaveman`. This can be
#' specified as two vectors x and y, a 2-column matrix x, a list with two components, etc.
#' @param method string. currently supports "concaveman".
#' @param ... Propagate to the method.
#' @param concavity numeric a relative measure of concavity. 1 results in a relatively detailed shape,
#' Infinity results in a convex hull. You can use values lower than 1, but they can produce pretty crazy
#' shapes.
#' @param length_threshold numeric. When a segment length is below this threshold, it stops being
#' considered for further detailed processing. Higher values result in simpler shapes.
#' @return A `sfc_POLYGON` from `sf` or a `data.frame` in the case of `concaveman`
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
#' hull = st_concave_hull(las, length_threshold = 10)
#' plot(hull)
NULL

#' @export
#' @rdname st_hull
st_concave_hull <- function(x, method = "concaveman",  ...) UseMethod("st_concave_hull")


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
st_concave_hull.LAS = function(x, method = "concaveman",  ...)
{
  if (method == "concaveman")
  {
    chull <- concaveman(x$X, x$Y, ...)
    chull <- sf::st_polygon(list(as.matrix(chull)))
    chull <- sf::st_sfc(chull, crs = st_crs(x))
    if (!sf::st_is_valid(chull)) chull <- sf::st_make_valid(chull)
    return(chull)
  }

  if (method == "concavetin")
  {
    chull <- concavetin(x, ...)
    chull <- sf::st_polygon(list(as.matrix(chull)))
    chull <- sf::st_sfc(chull, crs = st_crs(x))
    if (!sf::st_is_valid(chull)) chull <- sf::st_make_valid(chull)
    return(chull)
  }

  stop("Method not supported. It should be one of 'concaveman' or 'concavetin'")
}

#' @rdname st_hull
#' @export
concaveman <- function(x, y = NULL, concavity = 2, length_threshold = 0)
{
  stopifnot(is.numeric(concavity), is.numeric(length_threshold), length(concavity) == 1L, length(length_threshold) == 1L)

  if (is.null(y))
  {
    if (!is.matrix(x) && !is.data.frame(x) && !is.list(x)) stop("A matrix, a list, or a data.frame is expected.")
    if (dim(x)[2] != 2) stop("Two columns are expected.")

    if (is.matrix(x))
    {
      y <- x[,2]
      x <- x[,1]
    } else
    {
      y <- x[[2]]
      x <- x[[1]]
    }
  }

  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), length(x) >= 3L)

  h <- grDevices::chull(x, y) - 1
  return(cpp_concaveman(x, y, concavity, length_threshold, h))
}

concavetin <- function(las, max_length = 8)
{
  assert_is_a_number(max_length)
  assert_all_are_non_negative(max_length)

  if (npoints(las) < 3) stop("Impossible to triangulate fewer than 3 points.", call. = FALSE)

  # Triangulate the points and trim small triangles
  D <- tDelaunay(las, trim = max_length)
  X <- st_coordinates(las, z = FALSE)
  #geometry::trimesh(D,X, asp = 1)

  # Compute the contours of the triangulation
  hull <- tin_exterior_ring(D, X)
  return(hull)
}

tin_exterior_ring = function(D, X, s = 5)
{
  used <- N <- . <- P1 <- P2 <- NULL

  # Get the contours of the partial triangulation
  # *********************************************
  D <- data.table::as.data.table(D)

  # Order the Delaunay triangulation clockwise
  D = t(apply(D, 1, function(x)
  {
    p1 <- c(X[x[1], 1], X[x[1], 2], 1)
    p2 <- c(X[x[2], 1], X[x[2], 2], 1)
    p3 <- c(X[x[3], 1], X[x[3], 2], 1)
    M  <- rbind(p1, p2, p3)
    clockwise <-  det(M) < 0
    if (clockwise) return(x)
    return(c(x[3], x[2], x[1]))
  }))

  # Compute the vertices (edge of each triangle)
  n  <- nrow(D)
  p1 <- integer(n*3)
  p2 <- integer(n*3)
  for (i in 1:n)
  {
    j <- 3*i-2
    tri <- as.numeric(D[i,])
    p1[j]     <- tri[1]
    p2[j]     <- tri[2]
    p1[1 + j] <- tri[1]
    p2[1 + j] <- tri[3]
    p1[2 + j] <- tri[2]
    p2[2 + j] <- tri[3]
  }

  vertice  <- matrix(c(p1, p2), ncol = 2)
  svertice <- t(apply(vertice, 1, sort))
  svertice <- data.table::as.data.table(svertice)
  vertice  <- data.table::as.data.table(vertice)
  names(vertice) <- c("p1", "p2")

  # Filter the contour vertices. Contour vertices are those
  # that appear only once
  notcontour1  <- duplicated(svertice)
  notcontour2  <- duplicated(svertice, fromLast = TRUE)
  contour      <- vertice[!(notcontour1|notcontour2)]
  contour$used <- FALSE

  # Extract the polygons from the contour
  # *************************************
  # Initialization
  n  <- nrow(contour)
  p1 <- numeric(n)
  p2 <- numeric(n)

  # The first vertice initiates a polygon
  p1[1] <- contour$p1[1]
  p2[1] <- contour$p2[1]
  data.table::set(contour, 1L, 3L, TRUE)

  # Find the last vertice that closes the polygon
  j <- which(contour$p1 == p1[1] & contour$used == FALSE)
  s <- length(j) == 0L

  if (s) j <- which(contour$p2 == p1[1] & contour$used == FALSE)
  if (length(j) == 0L) stop("Internal error in tHull: please report this error (length(j) == 0L)")
  if (length(j) > 1) j <- j[1]

  if (s) {
    p1[n] <- contour$p1[j]
    p2[n] <- contour$p2[j]
  } else {
    p1[n] <- contour$p2[j]
    p2[n] <- contour$p1[j]
  }

  data.table::set(contour, j, 3L, TRUE)

  # Continue until we reach the last vertice
  for (i in 2:n)
  {
    j <- which(contour$p1 == p2[i - 1] & contour$used == FALSE)
    s <- length(j) == 0L

    if (s) j <- which(contour$p2 == p2[i - 1] & contour$used == FALSE)
    if (length(j) == 0L) stop("Internal Error: please report this error (tHull L147)")
    if (length(j) > 1) j <- j[1]

    if (!s)
    {
      p1[i] <- contour$p1[j]
      p2[i] <- contour$p2[j]
    }
    else
    {
      p1[i] <- contour$p2[j]
      p2[i] <- contour$p1[j]
    }

    data.table::set(contour, j, 3L, TRUE)

    if (p2[i] == p1[n]) {
      contour <- contour[used == FALSE]
      break
    }
  }

  orderedcontour <- data.table::data.table(p1, p2)
  orderedcontour <- orderedcontour[orderedcontour$p1 != 0,]

  XX <- data.frame(P1 = X[orderedcontour$p1,1], P2 = X[orderedcontour$p1,2])
  data.table::setDT(XX)

  # Fix self intersection
  XX[, N := .N , by = .(P1,P2)]
  XX[c(1, .N), N := 1]
  XX = XX[N == 1][, N := NULL][]

  # Close polygon
  XX <-rbind(XX, XX[1,])
  return(XX)
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
