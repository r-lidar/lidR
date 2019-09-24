tDelaunay = function(P, trim = 0, scales = c(1,1), offsets = c(0,0), option = "QbB")
{
  stopifnot(is.numeric(trim), length(trim) == 1L)

  if (inherits(P, "LAS")) {
    xscale  <- P@header@PHB[["X scale factor"]]
    yscale  <- P@header@PHB[["Y scale factor"]]
    xoffset <- P@header@PHB[["X offset"]]
    yoffset <- P@header@PHB[["Y offset"]]
    scales  <- c(xscale, yscale)
    offsets <- c(xoffset, yoffset)
    if (xscale == yscale) {
      D <- C_delaunay(P, scales, offsets)
    }
    else {
      Q <- as.matrix(coordinates(P))
      D <- suppressMessages(geometry::delaunayn(Q, options = option))
    }
  }
  else if (is.data.frame(P))
  {
    if (scales[1] == scales[2]) {
      D <- C_delaunay(P, scales, offsets)
    }
    else {
      Q <- as.matrix(P[,1:2])
      D <- suppressMessages(geometry::delaunayn(Q, options = option))
    }
  }
  else if (is.matrix(P)) {
    Q <- P
    if (ncol(P) > 2) {
      Q <- P[,1:2]
    }

    D <- suppressMessages(geometry::delaunayn(Q, options = option))
  }
  else {
    stop("Internal error. No method to triangulate this input", call. = FALSE)
  }

  if (trim != 0) {
    N <- tInfo(D, P)
    K <- N[,7] < abs(trim)
    if (trim < 0) K <- !K
    D <- D[K,]
  }

  return(D)
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
