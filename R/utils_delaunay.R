tDelaunay = function(P, trim = 0, scales = c(1,1), offsets = c(0,0), option = "QbB")
{
  stopifnot(is.numeric(trim), length(trim) == 1L)
  stopifnot(is.numeric(scales), length(scales) == 2L)
  stopifnot(is.numeric(offsets), length(offsets) == 2L)

  boosted_triangulation <- TRUE

  if (inherits(P, "LAS")) {
    xscale  <- P@header@PHB[["X scale factor"]]
    yscale  <- P@header@PHB[["Y scale factor"]]
    xoffset <- P@header@PHB[["X offset"]]
    yoffset <- P@header@PHB[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)
    P <- coordinates3D(P)
  }

  if (!is.data.frame(P) && !is.matrix(P)) {
    stop("Internal error. No method to triangulate this input", call. = FALSE)
  }

  if (is.matrix(P)) {
    boosted_triangulation <- FALSE
    Q <- if (ncol(P) > 2) P[,1:2] else P
  }

  if (boosted_triangulation) {
    if (scales[1] != scales[2]) {
      message("The Delaunay triangulation reverted to the old slow method because xy scale factors are different, so the fast method cannot be applied.")
      boosted_triangulation <- FALSE
      P <- as.matrix(P)
      Q <- as.matrix(P[,1:2])
    }
  }

  if (boosted_triangulation) {
    X <- P$X[1]
    Y <- P$Y[1]
    x <- (X - offsets[1]) / scales[1]
    y <- (Y - offsets[2]) / scales[2]

    if (abs(x - round(x)) > 1e-5 | abs(y - round(y)) > 1e-5) {
      message("The Delaunay triangulation reverted to the old slow method because xy coordinates were not converted to integer. xy scale factors and offsets are likely to be invalid")
      boosted_triangulation <- FALSE
      P <- as.matrix(P)
      Q <- as.matrix(P[,1:2])
    }
  }

  if (boosted_triangulation) {
    D <- C_delaunay(P, scales, offsets, trim)
    if (trim != 0) D <- D[!is.na(D[,1]),]
    return(D)
  }
  else {
    D <- suppressMessages(geometry::delaunayn(Q, options = option))

    if (trim != 0) {
      N <- tInfo(D, P)
      K <- N[,7] < abs(trim)
      if (trim < 0) K <- !K
      D <- D[K, , drop = FALSE]
    }

    return(D)
  }
}

# @rdname tDelaunay
tInterpolate = function(D, P, X, threads = 1L)
{
  stopifnot(is.matrix(D), is.matrix(P), is.matrix(X))

  I <- tSearch(D, P, X, threads)
  N <- tInfo(D, P)
  N <- N[I,1:4]
  Z <- -(X[,1] * N[,1] + X[,2] * N[,2] + N[,4]) / N[,3]
  return(Z)
}

# @rdname tDelaunay
tSearch = C_tsearch

# @rdname tDelaunay
tInfo =  C_tinfo
