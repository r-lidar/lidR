#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{rasterize_terrain} or \link{normalize_height}. It
#' implements an algorithm for spatial interpolation. Spatial interpolation is based on a Delaunay
#' triangulation, which performs a linear interpolation within each triangle. There are usually a
#' few points outside the convex hull, determined by the ground points at the very edge of the dataset,
#' that cannot be interpolated with a triangulation. Extrapolation can be performed with another algorithm.
#'
#' @param ... unused
#' @param extrapolate There are usually a few points outside the convex hull, determined by the ground
#' points at the very edge of the dataset, that cannot be interpolated with a triangulation.
#' Extrapolation is done using \link{knnidw} by default.
#'
#' @export
#'
#' @family dtm algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#' #plot(las)
#'
#' dtm = rasterize_terrain(las, algorithm = tin())
#'
#' #plot(dtm)
#' #plot_dtm3d(dtm)
#' @name dtm_tin
tin = function(..., extrapolate = knnidw(3,1,50))
{
  assert_is_algorithm_spi(extrapolate)
  extrapolate <- lazyeval::uq(extrapolate)

  f = function(las, where)
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "tin")
    z <- interpolate_delaunay(las, where, trim = 0, min_normal_z = 3e-2)

    # Extrapolate beyond the convex hull
    isna <- is.na(z)
    nnas <- sum(isna)
    if (nnas > 0)
    {
      lidR.context <- "spatial_interpolation"
      where2 <- data.frame(X = where$X[isna],  Y = where$Y[isna])
      zknn <- extrapolate(las, where2)
      z[isna] <- zknn
    }

    return(z)
  }

  f <- plugin_dtm(f, omp = TRUE)
  return(f)
}

#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{rasterize_terrain} or \link{normalize_height}. It implements an algorithm
#' for spatial interpolation. Interpolation is done using a k-nearest neighbour (KNN) approach with
#' an inverse-distance weighting (IDW).
#'
#' @param k integer. Number of k-nearest neighbours. Default 10.
#' @param p numeric. Power for inverse-distance weighting. Default 2.
#' @param rmax numeric. Maximum radius where to search for knn. Default 50.
#'
#' @export
#'
#' @family dtm algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' #plot(las)
#'
#' dtm = rasterize_terrain(las, algorithm = knnidw(k = 6L, p = 2))
#'
#' #plot(dtm)
#' #plot_dtm3d(dtm)
#' @name dtm_idw
knnidw = function(k = 10, p = 2, rmax = 50)
{
  k <- lazyeval::uq(k)
  p <- lazyeval::uq(p)
  rmax <- lazyeval::uq(rmax)

  f = function(las, where)
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "knnidw")
    return(interpolate_knnidw(las, where, k, p, rmax))
  }

  f <- plugin_dtm(f, omp = TRUE)
  return(f)
}

#' Spatial Interpolation Algorithm
#'
#' This function is made to be used in \link{rasterize_terrain} or \link{normalize_height}. It
#' implements an algorithm for spatial interpolation. Spatial interpolation is based on universal
#' kriging using the \link[gstat:krige]{krige} function from \code{gstat}. This method combines the
#' KNN approach with the kriging approach. For each point of interest it kriges the terrain using
#' the k-nearest neighbour ground points. This method is more difficult to manipulate but it is also
#' the most advanced method for interpolating spatial data.
#'
#' @param k numeric. Number of k-nearest neighbours. Default 10.
#' @param model A variogram model computed with \link[gstat:vgm]{vgm}. If NULL it performs an ordinary
#' or weighted least squares prediction.
#'
#' @export
#'
#' @family dtm algorithms
#'
#' @examples
#' \dontrun{
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' plot(las)
#'
#' dtm = rasterize_terrain(las, algorithm = kriging())
#'
#' plot(dtm)
#' plot_dtm3d(dtm)
#' }
#' @name dtm_kriging
kriging = function(model = gstat::vgm(.59, "Sph", 874), k = 10L)
{
  assert_package_is_installed("gstat")

  f = function(las, where)
  {
    assert_is_valid_context(LIDRCONTEXTSPI, "kriging")
    return(interpolate_kriging(las, where, model, k))
  }

  f <- plugin_dtm(f)
  return(f)
}

interpolate_knnidw = function(points, coord, k, p, rmax = 50)
{
  if (!inherits(points, "LAS")) {
    h <- rlas::header_create(points)
    points <- LAS(points, h, check = F)
  }

  force_autoindex(points) <- LIDRGRIDPARTITION
  return(C_knnidw(points, coord$X, coord$Y, k, p, rmax, getThread()))
}

interpolate_kriging = function(points, coord, model, k)
{
  X <- Y <- Z <- NULL
  if (!getOption("lidR.verbose")) sink(tempfile())
  if (inherits(points, "LAS")) points <- points@data
  x  <- gstat::krige(Z~X+Y, location = ~X+Y, data = points, newdata = coord, model, nmax = k)
  sink()
  return(x$var1.pred)
}

interpolate_delaunay <- function(points, coord, trim = 0, scales = c(1,1), offsets = c(0,0), options = "QbB", min_normal_z = 0)
{
  # /!\ TODO: triangulation does not respect spatial index and always use grid partition

  stopifnot(is.numeric(trim), length(trim) == 1L)
  stopifnot(is.numeric(scales), length(scales) == 2L)
  stopifnot(is.numeric(offsets), length(offsets) == 2L)
  stopifnot(is.data.frame(coord))

  boosted_triangulation <- TRUE

  if (inherits(points, "LAS")) {
    xscale  <- points[["X scale factor"]]
    yscale  <- points[["Y scale factor"]]
    xoffset <- points[["X offset"]]
    yoffset <- points[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)
    points  <- points@data
  }

  stopifnot(is.data.frame(points))

  if (scales[1] != scales[2]) {
    message("The Delaunay triangulation reverted to the old slow method because xy scale factors are different so the fast method cannot be applied.")
    boosted_triangulation <- FALSE
  }

  # Check if coordinates actually match the resolution
  # Check only 100 of them
  n <- min(100L, length(points$X))
  s <- as.integer(seq(1L, length(points$X), length.out = n))
  X <- points$X[s]
  Y <- points$Y[s]
  x <- fast_countunquantized(X, scales[1], offsets[1])
  y <- fast_countunquantized(Y, scales[2], offsets[2])

  if (x > 0 | y > 0)
  {
    message("The Delaunay triangulation reverted to the old slow method because xy coordinates were not convertible to integer values. xy scale factors and offsets are likely to be invalid")
    boosted_triangulation <- FALSE
  }

  if (boosted_triangulation) {
    return(C_interpolate_delaunay(points, coord, scales, offsets, trim, min_normal_z, getThreads()))
  }
  else {
    P <- as.matrix(points)
    X <- as.matrix(coord)
    D <- tDelaunay(P, trim = trim)
    return(tInterpolate(D, P, X, getThreads()))
  }
}
