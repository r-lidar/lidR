#' Sensor tracking algorithm
#'
#' This function is made to be used in \link{track_sensor}. It implements an algorithm from Roussel et
#' al. 2020 (see reference) for sensor tracking using multiple returns to estimate the positioning of
#' the sensor by computing the intersection in space of the lines passing through the first and last
#' returns.
#'
#' When multiple returns from a single pulse are detected, the sensor computes their positions as being
#' in the center of the footprint and thus all aligned. Because of that behavior, a line
#' drawn between and beyond those returns must cross the sensor. Thus, several consecutive pulses
#' emitted in a tight interval (e.g. 0.5 seconds) can be used to approximate an intersection
#' point in the sky that corresponds to the sensor position given that the sensor carrier hasn't
#' moved much during this interval. A weighted least squares method gives an approximation of the
#' intersection by minimizing the squared sum of the distances between the intersection point and all
#' the lines.
#'
#' @param interval numeric. Interval used to bin the gps times and group the pulses to compute
#' a position at a given timepoint t.
#' @param pmin integer. Minimum number of pulses needed to estimate a sensor position.
#' For a given interval, the sensor position is not computed if the number of pulses is lower than
#' \code{pmin}.
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#'
#' # pmin = 15 because it is an extremely tiny file
#' # strongly decimated to reduce its size. There are
#' # actually few multiple returns
#' flightlines <- track_sensor(las, Roussel2020(pmin = 15))
#'
#' plot(las@header)
#' plot(flightlines, add = TRUE)
#' @references Roussel Jean-Romain, Bourdon Jean-Francois, Achim Alexis, Range-based intensity
#' normalization of ALS data over forested areas using a sensor tracking method from multiple returns
#' (submitted)
#' @export
Roussel2020 = function(interval = 0.5, pmin = 50)
{
  assert_is_a_number(interval)
  assert_is_a_number(pmin)
  interval <- lazyeval::uq(interval)
  pmin <- lazyeval::uq(pmin)

  f = function(data)
  {
    assert_is_valid_context(LIDRCONTEXTTRK, "Roussel2020")

    . <- X <- Y <- Z <- ReturnNumber <- PointSourceID <- pulseID <- gpstime <- UserData <- SCORE <- npulses <- NULL
    # Generate the bins
    bins <- round_any(data$gpstime, interval)
    # Find the position P of the sensor in each interval
    P  <- data[, if (.N > 2*pmin) sensor_positions(X,Y,Z, ReturnNumber), by = .(gpstime = bins, PointSourceID = PointSourceID)]
    return(P)
  }

  class(f) <- LIDRALGORITHMTRK
  return(f)
}

#' Sensor tracking algorithm
#'
#' This function is made to be used in \link{track_sensor}. It implements an algorithm from Gatziolis
#' and McGaughey 2019 (see reference) for sensor tracking using multiple returns to estimate the positioning
#' of the sensor by computing the intersection in space of the lines passing through the first and
#' last returns.
#'
#' In the original paper, two steps are described: (1) closest point approach (CPA) and (2) cubic
#' spline fitting. Technically, the cubic spline fitting step is a post-processing step and is not
#' included in this algorithm.\cr\cr
#' The source code of the algorithm is a slight modification of the original source code provided
#' with the paper to fit with the lidR package.
#'
#' @param SEGLENFactor scalar. Weighting factor for the distance b/w 1st and last pulse returns
#' @param AngleFactor scalar. Weighting factor for view angle of mother pulse of a return
#' @param deltaT scalar. TimeBlock duration (in seconds)
#' @references Gatziolis, D., & McGaughey, R. J. (2019). Reconstructing Aircraft Trajectories from
#' Multi-Return Airborne Laser-Scanning Data. Remote Sensing, 11(19), 2258.
#' @examples
#' # A valid file properly populated
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile)
#' flightlines <- track_sensor(las, Gatziolis2019())
#'
#' plot(las@header)
#' plot(flightlines, add = TRUE)
#' @author Demetrios Gaziolis and Jean-Romain Roussel
#' @export
Gatziolis2019 <- function(SEGLENFactor = 1.0059, AngleFactor = 0.8824, deltaT = 0.5)
{
  assert_is_a_number(SEGLENFactor)
  assert_is_a_number(AngleFactor)
  assert_is_a_number(deltaT)

  f = function(data)
  {
    assert_is_valid_context(LIDRCONTEXTTRK, "Gatziolis2019")

    XFIRST <- XLAST <- YFIRST <- YLAST <- ZFIRST <- ZLAST <- SEGLEN <- SCANANGLE <- SCORE <- TBLOCK <- WT <- PULSEFLAG <- DIST <- NULL

    if ("ScanAngleRank" %in% names(data))
      angleattribute = "ScanAngleRank"
    else if ("ScanAngle" %in% names(data))
      angleattribute = "ScanAngle"
    else
      stop("Error: Scan angles are missing", call. = FALSE)

    select = c("X", "Y", "Z", "gpstime", angleattribute, "PointSourceID")

    # Reshape the data to get one pulse (pair of points) per rows
    v <- seq( 2, nrow(data), by = 2L)
    pulse.dt <- data.table::as.data.table(data[v, c("X", "Y", "Z")])
    names(pulse.dt) <- c("XFIRST", "YFIRST", "ZFIRST")
    pulse.dt[, c("XLAST", "YLAST", "ZLAST", "T", "SCANANGLE", "PointSourceID") := data[v-1, select, with = FALSE]]
    pulse.dt[, SEGLEN := sqrt( (XFIRST - XLAST)^2 + (YFIRST - YLAST)^2 + (ZFIRST - ZLAST)^2 ) ]

    ## Define time blocks of pulses
    minT <- min(pulse.dt[, "T"])
    minT <- floor(minT / deltaT) * deltaT                 ## aligns minT to increments of deltaT
    pulse.dt[, TBLOCK := floor( (T - minT) / deltaT )]    ## generates time blocks of returns labeled 0 to k.
    maxT <- ceiling(max(pulse.dt[, "T"]) / deltaT) * deltaT

    # >>>>>>>>>>>>>>>>
    # (Removed from original code because it is already performed upstream)
    #
    ## check if at least two eligible pulses are present in each timeblock. If only one, remove it
    #tblBlock.df <- count(pulse.dt$TBLOCK)
    #w <- tblBlock.df$freq > 1 ## you need at least two pulses to compute a platform location estimate
    #pulse.dt <- subset(pulse.dt, TBLOCK %in% tblBlock.df$x[w])
    # <<<<<<<<<<<<<<<<

    ## compute pulse weights (and convert pulses with negative SCANANGLE to negative weight)
    pulse.dt[, WT := exp(SEGLENFactor)*SEGLEN + exp(AngleFactor)*abs(SCANANGLE)]
    pulse.dt[SCANANGLE < 0, WT := -1.0 * WT]

    ## Filter to two 'best' pulses per time Block
    pulseflag = filterTimeBlockPulses(pulse.dt)
    pulse.dt[, PULSEFLAG := pulseflag ]
    pulse.dt <- pulse.dt[PULSEFLAG == 1][, PULSEFLAG := NULL][]

    ## calculate cpa
    trj.df <- cmpCPA(pulse.dt)
    data.table::setDT(trj.df)
    trj.df[, DIST := NULL]
    data.table::setcolorder(trj.df, c("T", "PointSourceID", "X", "Y", "Z", "WT"))
    data.table::setnames(trj.df, c("gpstime", "PointSourceID", "X", "Y", "Z", "SCORE"))
    return(trj.df)
  }

  class(f) <- LIDRALGORITHMTRK
  return(f)
}

sensor_positions <- function(x, y, z, rn)
{
  first <- rn == 1L
  last  <- rn > 1L

  # Adapted from MATLAB (Anders Eikenes, 2012)
  # http://www.mathworks.com/matlabcentral/fileexchange/37192-intersection-point-of-lines-in-3d-space

  # Matrix n x 3 containing XYZ coordinates for each first/last return
  A <- matrix(c(x[first], y[first], z[first]), ncol = 3L)
  B <- matrix(c(x[last], y[last], z[last]), ncol = 3L)

  # This should never happen but it might happen in wrongly populated datasets
  if (nrow(A) != nrow(B))
    return(list(X = NA_real_, Y = NA_real_, Z = NA_real_, npulses = NA_integer_))

  # Lines described as vectors V
  V <- B - A

  # Weights lines by distances
  D <- matrix(sqrt(.rowSums(V^2, nrow(V), ncol(V))))
  W <- D / sum(D)

  # Normalized vectors (|V| = 1)
  ones <- matrix(1, ncol = 3)
  N  <- V / (D %*% ones)
  Nx <- N[, 1L, drop = FALSE]
  Ny <- N[, 2L, drop = FALSE]
  Nz <- N[, 3L, drop = FALSE]

  Wxx <- W * (Nx^2 - 1)
  Wyy <- W * (Ny^2 - 1)
  Wzz <- W * (Nz^2 - 1)
  Wxy <- W * Nx * Ny
  Wxz <- W * Nx * Nz
  Wyz <- W * Ny * Nz

  Sxx <- sum(Wxx)
  Syy <- sum(Wyy)
  Szz <- sum(Wzz)
  Sxy <- sum(Wxy)
  Sxz <- sum(Wxz)
  Syz <- sum(Wyz)
  S   <- matrix(c(Sxx, Sxy, Sxz, Sxy, Syy, Syz, Sxz, Syz, Szz), nrow = 3L)

  Cx <- sum(A[,1, drop = FALSE] * Wxx + A[,2, drop = FALSE] * Wxy + A[,3, drop = FALSE] * Wxz)
  Cy <- sum(A[,1, drop = FALSE] * Wxy + A[,2, drop = FALSE] * Wyy + A[,3, drop = FALSE] * Wyz)
  Cz <- sum(A[,1, drop = FALSE] * Wxz + A[,2, drop = FALSE] * Wyz + A[,3, drop = FALSE] * Wzz)
  C  <- matrix(c(Cx,Cy,Cz))

  M  <- t(solve(S,C))
  M  <- round(M, 3)

  return(list(X = M[1], Y = M[2], Z = M[3], SCORE = nrow(A)))
}
