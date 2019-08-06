# Delaunay triangulation tools
#
# Internal functions: a set of tools to work with Delaunay triangulation.
# \itemize{
# \item \code{tDelaunay}: Delaunay triangulation (DT) with triangle size filter
# \item \code{tInterpolate}: linear interpolation within DT at coordinates X.
# \item \code{tSerach}: finds the index of the triangule in DT containing the points X.
# \item \code{tInfo}: compute informations for each triangle including normal vector, area, longest
# edge.
# }
#
# @param P matrix. Points triangulated or to be triangulated.
# @param D matrix. Triangulation
# @param X matrix. Points to test.
# @param trim numeric. If \code{trim > 0}, removes all the triangles that have the longest edge greater than
# this value. If \code{trim < 0}, the same but smaller than this value.
# @param threads integer. Number of OpenMP threads.
#
# @keywords internal
# @examples
# P <- matrix(runif(30, 0, 10), ncol = 3)
# X <- matrix(runif(20, 0, 10), ncol = 2)
# D <- tDelaunay(P, trim = -4)
# geometry::trimesh(D, P)
# points(X)
# tInfo(D,P)
# tInterpolate(D, P, X)
tDelaunay = function(P, trim = 0, option = "QbB")
{
  stopifnot(is.numeric(trim), length(trim) == 1L)

  if (ncol(P) > 2)
    P <- P[,1:2]

  D <- suppressMessages(geometry::delaunayn(P, options = option))

  if (trim != 0)
  {
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
  I <- tSearch(D, P, X, threads)
  N <- tInfo(D, P)
  N <- N[I,1:4]
  Z <- -(X[,1] * N[,1] + X[,2] * N[,2] + N[,4]) / N[,3]
  return(Z)
}

# @rdname tDelaunay
tSearch = function(D, P, X, threads = 1L)
{
  C_tsearch(P[,1], P[,2], D, X[,1], X[,2], threads)
}

# @rdname tDelaunay
tInfo = function(D, P)
{
  C_tinfo(D, P)
}
