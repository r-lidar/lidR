# ==== STANDARD ====

#' Predefined standard metrics functions
#'
#' Predefined metrics functions intended to me used in `*_metrics` function such as
#' \link{pixel_metrics}, \link{cloud_metrics}, \link{crown_metrics}, \link{voxel_metrics} and
#' so on. Each function comes with a convenient shortcuts for lazy coding. The `lidR` package aims
#' to provide an easy way to compute user-defined metrics rather than to provide them. However, for
#' efficiency and to save time, sets of standard metrics have been predefined (see details). Every
#' function can be computed by every `*_metrics` functions however `stdmetrics*` are
#' more pixel-based metrics, `stdtreemetrics` are more tree-based metrics and `stdshapemetrics` are
#' more point-based metrics. For example the metric `zmean` computed by `stdmetrics_z` makes sense
#' when computed at the pixel level but brings no information at the voxel level.
#'
#' The function names, their parameters and the output names of the metrics rely on a nomenclature
#' chosen for brevity:
#' \itemize{
#' \item{`z`: refers to the elevation}
#' \item{`i`: refers to the intensity}
#' \item{`rn`: refers to the return number}
#' \item{`q`: refers to quantile}
#' \item{`a`: refers to the ScanAngleRank or ScanAngle}
#' \item{`n`: refers to a number (a count)}
#' \item{`p`: refers to a percentage}
#' }
#' For example the metric named `zq60` refers to the elevation, quantile, 60 i.e. the 60th percentile
#' of elevations. The metric `pground` refers to a percentage. It is the percentage of points
#' classified as ground. The function `stdmetric_i` refers to metrics of intensity. A description of
#' each existing metric can be found on the \href{https://github.com/r-lidar/lidR/wiki/stdmetrics}{lidR wiki page}.\cr\cr
#' Some functions have optional parameters. If these parameters are not provided the function
#' computes only a subset of existing metrics. For example, `stdmetrics_i` requires the intensity
#' values, but if the elevation values are also provided it can compute additional metrics such as
#' cumulative intensity at a given percentile of height.\cr\cr
#' Each function has a convenient associated variable. It is the name of the function, with a
#' dot before the name. This enables the function to be used without writing parameters. The cost
#' of such a feature is inflexibility. It corresponds to a predefined behaviour (see examples)\cr
#' \describe{
#' \item{`stdmetrics`}{is a combination of `stdmetrics_ctrl` + `stdmetrics_z` +
#' `stdmetrics_i` +  `stdmetrics_rn`}
#' \item{`stdtreemetrics`}{is a special function that works with \link{crown_metrics}. Actually,
#' it won't fail with other functions but the output makes more sense if computed at the
#' individual tree level.}
#' \item{`stdshapemetrics`}{is a set of eigenvalue based feature described in Lucas et al, 2019
#' (see references).}
#' }
#'
#' @param x,y,z,i Coordinates of the points, Intensity
#' @param rn,class ReturnNumber, Classification
#' @param pulseID The number referencing each pulse
#' @param dz numeric. Layer thickness  metric \link[=entropy]{entropy}
#' @param th numeric. Threshold for metrics pzabovex. Can be a vector to compute with several thresholds.
#' @param zmin numeric. Lower bound of the integral for zpcumx metrics.
#' See \href{https://github.com/r-lidar/lidR/wiki/stdmetrics}{wiki page} and Wood et al.
#' (2008) reference.
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "*", filter = "-keep_random_fraction 0.5")
#'
#' # All the predefined metrics
#' m1 <- pixel_metrics(las, ~stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification,dz=1), res = 40)
#'
#' # Convenient shortcut
#' m2 <- pixel_metrics(las, .stdmetrics, res = 40)
#'
#' # Basic metrics from intensities
#' m3 <- pixel_metrics(las, ~stdmetrics_i(Intensity), res = 40)
#'
#' # All the metrics from intensities
#' m4 <- pixel_metrics(las, ~stdmetrics_i(Intensity, Z, Classification, ReturnNumber), res = 40)
#'
#' # Convenient shortcut for the previous example
#' m5 <- pixel_metrics(las, .stdmetrics_i, res = 40)
#'
#' # Combine some predefined function with your own new metrics
#' # Here convenient shortcuts are no longer usable.
#' myMetrics = function(z, i, rn)
#' {
#'   first  <- rn == 1L
#'   zfirst <- z[first]
#'   nfirst <- length(zfirst)
#'   above2 <- sum(z > 2)
#'
#'   x <- above2/nfirst*100
#'
#'   # User's metrics
#'   metrics <- list(
#'      above2aboven1st = x,       # Num of returns above 2 divided by num of 1st returns
#'      zimean  = mean(z*i),       # Mean products of z by intensity
#'      zsqmean = sqrt(mean(z^2))  # Quadratic mean of z
#'    )
#'
#'   # Combined with standard metrics
#'   return( c(metrics, stdmetrics_z(z)) )
#' }
#'
#' m10 <- pixel_metrics(las, ~myMetrics(Z, Intensity, ReturnNumber), res = 40)
#'
#' # Users can write their own convenient shorcuts like this:
#' .myMetrics = ~myMetrics(Z, Intensity, ReturnNumber)
#' m11 <- pixel_metrics(las, .myMetrics, res = 40)
#'
#' @references
#' M. Woods, K. Lim, and P. Treitz. Predicting forest stand variables from LiDAR data in
#' the Great Lakes – St. Lawrence forest of Ontario. The Forestry Chronicle. 84(6): 827-839.
#' https://doi.org/10.5558/tfc84827-6
#' @rdname stdmetrics
#' @export
#' @md
stdmetrics = function(x, y, z, i, rn, class, dz = 1, th = 2, zmin = 0)
{
  C  <- stdmetrics_ctrl(x, y, z)
  Z  <- stdmetrics_z(z, dz, th, zmin)
  I  <- stdmetrics_i(i, z, class, rn)
  RN <- stdmetrics_rn(rn, class)
  #PU = stdmetrics_pulse(pulseID, rn)

  metrics <- c(Z, I, RN, C)
  return(metrics)
}

#' @rdname stdmetrics
#' @export
stdmetrics_z = function(z, dz = 1, th = 2, zmin = 0)
{
  n <- length(z)
  zmax  <- max(z)
  zmean <- mean(z)

  probs <- seq(0.05, 0.95, 0.05)
  zq 	  <- as.list(stats::quantile(z, probs))
  names(zq) <- paste0("zq", probs*100)

  pzabovex <- lapply(th, function(x) { fast_countover(z, x) / n * 100 })
  names(pzabovex) <- paste0("pzabove", th)

  pzabovemean <- fast_countover(z, zmean) / n * 100

  if (zmax <= zmin)
  {
    d <- rep(0, 9)
  }
  else
  {
    breaks <- seq(zmin, zmax, (zmax-zmin)/10)
    d <- findInterval(z[z>zmin], breaks)
    d <- fast_table(d, 10)
    d <- d / sum(d)*100
    d <- cumsum(d)[1:9]
    d <- as.list(d)
  }
  names(d) <- paste0("zpcum", 1:9)

  metrics <- list(
    zmax  = zmax,
    zmean = zmean,
    zsd   = stats::sd(z),
    zskew = (sum((z - zmean)^3)/n)/(sum((z - zmean)^2)/n)^(3/2),
    zkurt = n * sum((z - zmean)^4)/(sum((z - zmean)^2)^2),
    zentropy  = entropy(z, dz),
    pzabovezmean = pzabovemean
  )

  metrics <- c(metrics, pzabovex, zq, d)

  return(metrics)
}

#' @rdname stdmetrics
#' @export
stdmetrics_i = function(i, z = NULL, class = NULL, rn = NULL)
{
  itot <- imax <- imean <- isd <- icv <- iskew <- ikurt <- NULL
  icumzq10 <- icumzq30 <- icumzq50 <- icumzq70 <- icumzq90 <- NULL

  n <- length(i)
  itot <- as.double(sum(i))
  imean <- mean(i)

  probs <- seq(0.05, 0.95, 0.05)
  iq 	  <- as.list(stats::quantile(i, probs))
  names(iq) <- paste("iq", probs*100, sep = "")

  metrics <- list(
    itot = itot,
    imax  = max(i),
    imean = imean,
    isd   = stats::sd(i),
    iskew = (sum((i - imean)^3)/n)/(sum((i - imean)^2)/n)^(3/2),
    ikurt = n * sum((i - imean)^4)/(sum((i - imean)^2)^2)
  )

  if (!is.null(class))
  {
    metrics <- c(metrics, list(ipground = sum(i[class == 2])/itot*100))
  }

  if (!is.null(z))
  {
    zq <- stats::quantile(z, probs = seq(0.1, 0.9, 0.2))

    ipcum <- list(
      ipcumzq10 = sum(i[z <= zq[1]])/itot*100,
      ipcumzq30 = sum(i[z <= zq[2]])/itot*100,
      ipcumzq50 = sum(i[z <= zq[3]])/itot*100,
      ipcumzq70 = sum(i[z <= zq[4]])/itot*100,
      ipcumzq90 = sum(i[z <= zq[5]])/itot*100
    )

    metrics <- c(metrics, ipcum)
  }

  metrics
}

#' @rdname stdmetrics
#' @export
stdmetrics_rn = function(rn, class = NULL)
{
  nground <- NULL

  n <- length(rn)

  prn <- table(factor(rn, levels = c(1:5)))/n*100
  prn <- as.list(prn)
  names(prn) <- paste0("p", names(prn), "th")

  metrics <- prn

  if (!is.null(class))
    metrics = c(metrics, list(pground = sum(class == 2)/n*100))

  return(metrics)
}

#' @rdname stdmetrics
#' @export
stdmetrics_pulse = function(pulseID, rn)
{
  . <- nr <- NULL

  n <- length(rn)

  dt <- data.table::data.table(pulseID, rn)

  np_with_x_return <- dt[, .(nr = max(rn)), by = pulseID][, .N, by = nr]
  data.table::setorder(np_with_x_return, nr)

  np <- numeric(9)
  names(np) <- paste0("ppulse", 1:9, "return")
  np[np_with_x_return$nr] <- np_with_x_return$N/n*100
  np <- as.list(np)

  return(np)
}

#' @rdname stdmetrics
#' @export
stdmetrics_ctrl = function(x, y, z)
{
  xmax <- max(x)
  ymax <- max(y)
  xmin <- min(x)
  ymin <- min(y)
  n    <- length(z)
  area <- (xmax - xmin)*(ymax - ymin)
  metrics <- list(n = n, area = area)

  return(metrics)
}

#' @rdname stdmetrics
#' @export
stdtreemetrics = function(x, y, z)
{
  metrics <- list(
    Z = max(z),
    npoints = length(x),
    convhull_area = round(area_convex_hull(x,y),3)
  )

  return(metrics)
}
#' @rdname stdmetrics
#' @export
#' @references
#' Lucas, C., Bouten, W., Koma, Z., Kissling, W. D., & Seijmonsbergen, A. C. (2019). Identification
#' of Linear Vegetation Elements in a Rural Landscape Using LiDAR Point Clouds. Remote Sensing, 11(3), 292.
stdshapemetrics = function(x,y,z)
{
  xyz     <- cbind(x,y,z)
  eigen   <- fast_eigen_values(xyz)
  eigen_m <- eigen[[1]]
  eigen_m <- as.numeric(eigen_m)

  shapemetrics <- list(
    eigen_largest  = eigen_m[1],
    eigen_medium   = eigen_m[2],
    eigen_smallest = eigen_m[3],
    curvature      = eigen_m[3]/(eigen_m[1] + eigen_m[2] + eigen_m[3]),
    linearity      = (eigen_m[1] - eigen_m[2])/eigen_m[1],
    planarity      = (eigen_m[2] - eigen_m[3])/eigen_m[1],
    sphericity     = eigen_m[3]/eigen_m[1],
    anisotropy     = (eigen_m[1] - eigen_m[3])/eigen_m[1],
    horizontality  = abs(eigen[[2]][3,3])
  )
  return(shapemetrics)
}

stdtreeapex <- function(x,y,z, ...)
{
  if (length(x) < 4)
    return(NULL)

  j <- which.max(z)
  point <- sf::st_point(c(x[j], y[j], z[j]))
  return(list(geom = list(point)))
}

stdtreehullconvex = function(x,y,z, ...)
{
  if (length(x) < 4)
    return(NULL)

  i <- grDevices::chull(x,y)
  i <- c(i, i[1])
  P <- cbind(x[i], y[i])
  poly <- sf::st_polygon(list(P))

  return(list(geom = list(poly)))
}

stdtreehullconcave = function(x,y,z, concavity, length_threshold)
{
  if (length(x) < 4)
    return(NULL)

  P <- concaveman(x, y, concavity, length_threshold)
  P <- as.matrix(P)
  poly <- sf::st_polygon(list(P))

  return(list(geom = list(poly)))
}

stdtreehullbbox = function(x,y,z, ...)
{
  if (length(x) < 4)
    return(NULL)

  xmin <- min(x)
  ymin <- min(y)
  xmax <- max(x)
  ymax <- max(y)

  x <- c(xmin, xmax, xmax, xmin, xmin)
  y <- c(ymin, ymin, ymax, ymax, ymin)
  P <- cbind(x, y)
  poly <- sf::st_polygon(list(P))

  return(list(geom = list(poly)))
}

#' @rdname stdmetrics
#' @export
.stdmetrics = ~stdmetrics(X,Y,Z,Intensity, ReturnNumber, Classification, dz = 1, th = 2)

#' @rdname stdmetrics
#' @export
.stdmetrics_z = ~stdmetrics_z(Z, dz =1)

#' @rdname stdmetrics
#' @export
.stdmetrics_i = ~stdmetrics_i(Intensity, Z, Classification, ReturnNumber)

#' @rdname stdmetrics
#' @export
.stdmetrics_rn = ~stdmetrics_rn(ReturnNumber, Classification)

#' @rdname stdmetrics
#' @export
.stdmetrics_pulse = ~stdmetrics_pulse(pulseID, ReturnNumber)

#' @rdname stdmetrics
#' @export
.stdmetrics_ctrl = ~stdmetrics_ctrl(X, Y, Z)

#' @rdname stdmetrics
#' @export
.stdtreemetrics = ~stdtreemetrics(X, Y, Z)

#' @rdname stdmetrics
#' @export
.stdshapemetrics = ~stdshapemetrics(X, Y, Z)

# ===== OTHER =====

#' Predefined non standard metrics
#'
#' Functions and metrics from the literature. See details and references
#'
#' \describe{
#' \item{rumple_index}{Computes the roughness of a surface as the ratio between its area and its
#' projected area on the ground. If the input is a gridded object (raster) the function computes the
#' surfaces using Jenness's algorithm (see references). If the input is a point cloud the function
#' uses a Delaunay triangulation of the points and computes the area of each triangle.}
#' \item{gap_fraction_profile}{Computes the gap fraction profile using the method of Bouvier et al.
#' (see reference). The function assesses the number of laser points that actually reached the layer
#' z+dz and those that passed through the layer [z, z+dz]. By definition the layer 0
#' will always return 0 because no returns pass through the ground. Therefore, the layer 0 is removed
#' from the returned results.}
#' \item{LAD}{Computes a leaf area density profile based on the method of Bouvier et al. (see reference)
#' The function assesses the number of laser points that actually reached the layer z+dz and those that
#' passed through the layer [z, z+dz] (see \link[=gap_fraction_profile]{gap_fraction_profile}).
#' Then it computes the log of this quantity and divides it by the extinction coefficient k as
#' described in Bouvier et al. By definition the layer 0 will always return infinity because no returns
#'  pass through the ground. Therefore, the layer 0 is removed from the returned results.}
#'  \item{entropy}{A normalized Shannon vertical complexity index. The Shannon diversity index is a
#'  measure for quantifying diversity and is based on the number and frequency of species present. This index,
#' developed by Shannon and Weaver for use in information theory, was successfully transferred
#' to the description of species diversity in biological systems (Shannon 1948). Here it is applied
#' to quantify the diversity and the evenness of an elevational distribution of las points. It
#' makes bins between 0 and the maximum elevation. If there are negative values the function
#' returns NA.}
#' \item{VCI}{Vertical Complexity Index. A fixed normalization of the entropy function from van Ewijk
#' et al. (2011) (see references)}
#' }
#'
#' @name nstdmetrics
#' @rdname nstdmetrics
#' @param by,dz numeric. The thickness of the layers used (height bin)
#' @param zmax numeric. Maximum elevation for an entropy normalized to zmax.
NULL

#' @param x A `RasterLayer`, a `stars` a `SpatRaster` or a vector of x coordinates.
#' @param y numeric. If `x` is a vector of coordinates: the associated y coordinates.
#' @param z numeric. If `x` is a vector of coordinates: the associated z coordinates.
#' @param ... unused
#' @return numeric. The computed Rumple index.
#'
#' @export
#' @examples
#' x <- runif(20, 0, 100)
#' y <- runif(20, 0, 100)
#'
#' # Perfectly flat surface, rumple_index = 1
#' z <- rep(10, 20)
#' rumple_index(x, y, z)
#'
#' # Rough surface, rumple_index > 1
#' z <- runif(20, 0, 10)
#' rumple_index(x, y, z)
#'
#' # Rougher surface, rumple_index increases
#' z <- runif(20, 0, 50)
#' rumple_index(x, y, z)
#'
#' # Measure of roughness is scale-dependent
#' rumple_index(x, y, z)
#' rumple_index(x/10, y/10, z)
#'
#' # Use with a canopy height model
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile)
#' chm <- rasterize_canopy(las, 2, p2r())
#' rumple_index(chm)
#'
#' @md
#' @references
#' Jenness, J. S. (2004). Calculating landscape surface area from digital elevation
#' models. Wildlife Society Bulletin, 32(3), 829–839.
#' @rdname nstdmetrics
rumple_index = function(x, y = NULL, z = NULL, ...)
{
  UseMethod("rumple_index", x)
}

#' @export
rumple_index.RasterLayer <- function(x, y = NULL, z = NULL, ...)
{
  res <- raster_res(x)
  x   <- raster_as_matrix(x)$z
  return(rumple_index.matrix(x, res[1], res[2]))
}

#' @export
rumple_index.stars <- function(x, y = NULL, z = NULL, ...)
{
  res <- raster_res(x)
  x   <- raster_as_matrix(x)$z
  return(rumple_index.matrix(x, res[1], res[2]))
}

#' @export
rumple_index.SpatRaster <- function(x, y = NULL, z = NULL, ...)
{
  res <- raster_res(x)
  x   <- raster_as_matrix(x)$z
  return(rumple_index.matrix(x, res[1], res[2]))
}


rumple_index.matrix <- function(x, y = NULL, z = NULL, ...)
{
  area  <- sp::surfaceArea(x, y, z)
  parea <- sum(!is.na(x))*y*z
  return(area/parea)
}

#' @export
rumple_index.numeric <- function(x, y = NULL, z = NULL, ...)
{
  assert_is_numeric(x)
  assert_is_numeric(y)
  assert_is_numeric(z)
  assert_are_same_length(x,y)
  assert_are_same_length(x,z)

  if (length(x) <= 3)
    return(NA_real_)

  rumple <- tryCatch(
  {
    X <- cbind(x,y,z)
    dn <- suppressMessages(geometry::delaunayn(X[,1:2], options = "QbB"))
    N <- C_tinfo(dn, X)
    area  <- sum(N[,5])
    parea <- sum(N[,6])
    return(area/parea)
  },
  error = function(e)
  {
    message(paste0(e, "\n'rumple_index' returned NA."))
    if (getOption("lidR.debug")) dput(X[,1:2])
    return(NA_real_)
  })

  return(rumple)
}

#' @param z vector of positive z coordinates
#' @param z0 numeric. The bottom limit of the profile
#' @return A data.frame containing the bin elevations (z) and the gap fraction for each bin (gf)
#' @examples
#' z <- c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z <- z[z<45 & z>0]
#'
#' hist(z, n=50)
#'
#' gapFraction = gap_fraction_profile(z)
#'
#' plot(gapFraction, type="l", xlab="Elevation", ylab="Gap fraction")
#' @references Bouvier, M., Durrieu, S., Fournier, R. a, & Renaud, J. (2015).  Generalizing predictive
#' models of forest inventory attributes using an area-based approach with airborne las data. Remote
#' Sensing of Environment, 156, 322-334. http://doi.org/10.1016/j.rse.2014.10.004
#' @export
#' @rdname nstdmetrics
gap_fraction_profile = function(z, dz = 1, z0 = 2)
{
  bk <- seq(floor((min(z) - z0)/dz)*dz + z0, ceiling((max(z) - z0)/dz)*dz + z0, dz)

  if (length(bk) <= 1)
    return(data.frame(z = numeric(0), gf = numeric(0)))

  histogram <- graphics::hist(z, breaks = bk, plot = F)
  h <- histogram$mids
  p <- histogram$counts/sum(histogram$counts)

  p <- c(p, 0)

  cs <- cumsum(p)
  i <- data.table::shift(cs)/cs
  i[is.na(i)] = 0

  i[is.nan(i)] = NA

  z = h #[-1]
  i = i[-length(i)] #[-c(1, length(i))]

  return(data.frame(z = z[z > z0], gf = i[z > z0]))
}


#' @param k numeric. is the extinction coefficient
#' @examples
#' z <- c(rnorm(1e4, 25, 6), rgamma(1e3, 1, 8)*6, rgamma(5e2, 5,5)*10)
#' z <- z[z<45 & z>0]
#'
#' lad <- LAD(z)
#'
#' plot(lad, type="l", xlab="Elevation", ylab="Leaf area density")
#' @export
#' @rdname nstdmetrics
LAD = function(z, dz = 1, k = 0.5, z0 = 2) # (Bouvier et al. 2015)
{
  ld <- gap_fraction_profile(z, dz, z0)

  if (nrow(ld) <= 2)
    return(data.frame(z = numeric(0), lad = numeric(0)))

  if (anyNA(ld))
    return(data.frame(z = numeric(0), lad = numeric(0)))

  lad <- ld$gf
  lad <- -log(lad)/(k*dz)

  lad[is.infinite(lad)] = NA

  lad <- lad

  return(data.frame(z = ld$z, lad = lad))
}

#' @examples
#' z <- runif(10000, 0, 10)
#'
#' # expected to be close to 1. The highest diversity is given for a uniform distribution
#' entropy(z, by = 1)
#'
#'  z <- runif(10000, 9, 10)
#'
#' # Must be 0. The lowest diversity is given for a unique possibility
#' entropy(z, by = 1)
#'
#' z <- abs(rnorm(10000, 10, 1))
#'
#' # expected to be between 0 and 1.
#' entropy(z, by = 1)
#' @references
#' Pretzsch, H. (2008). Description and Analysis of Stand Structures. Springer Berlin Heidelberg. http://doi.org/10.1007/978-3-540-88307-4 (pages 279-280)
#' Shannon, Claude E. (1948), "A mathematical theory of communication," Bell System Tech. Journal 27, 379-423, 623-656.
#' @return A number between 0 and 1
#' @export
#' @rdname nstdmetrics
entropy = function(z, by = 1, zmax = NULL)
{
  # Fixed entropy (van Ewijk et al. (2011)) or flexible entropy
  if (is.null(zmax))
    zmax = max(z)

  # If zmax < 3 it is meaningless to compute entropy
  if (zmax < 2 * by)
    return(NA_real_)

  if (min(z) < 0)
    return(NA_real_)

  # Define the number of x meters bins from 0 to zmax (rounded to the next integer)
  bk <- seq(0, ceiling(zmax/by)*by, by)

  # Compute the p for each bin
  hist <- findInterval(z, bk)
  hist <- fast_table(hist, length(bk) - 1)
  hist <- hist/sum(hist)

  # Remove bins where there are no points because of log(0)
  p    <- hist[hist > 0]
  pref <- rep(1/length(hist), length(hist))

  # normalized entropy
  S <- -sum(p*log(p)) / -sum(pref*log(pref))

  return(S)
}


#' @return A number between 0 and 1
#' @examples
#' z <- runif(10000, 0, 10)
#'
#' VCI(z, by = 1, zmax = 20)
#'
#' z <- abs(rnorm(10000, 10, 1))
#'
#' # expected to be closer to 0.
#' VCI(z, by = 1, zmax = 20)
#' @references van Ewijk, K. Y., Treitz, P. M., & Scott, N. A. (2011). Characterizing Forest Succession in Central Ontario using LAS-derived Indices. Photogrammetric Engineering and Remote Sensing, 77(3), 261-269. Retrieved from <Go to ISI>://WOS:000288052100009
#' @export
#' @rdname nstdmetrics
VCI = function(z, zmax, by = 1)
{
  z <- z[z < zmax]
  return(entropy(z, by, zmax))
}
