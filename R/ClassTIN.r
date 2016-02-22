#' Triangular irregular network
#'
#' Triangular irregular network computed from Delaunay triangulation
#' @param x numeric. n points x coordinates
#' @param y numeric. n points y coordinates
#' @param z numeric. n points z coordinates
#' @return A data.frame containig the list of triangles coordinates in each row
#' @examples
#' x = runif(20)
#' y = runif(20)
#' z = runif(20)
#' tin = TIN(x, y, z)
#' plot(tin)
#' @seealso
#' \link[geometry:delaunayn]{delaunayn}
#' @export TIN
#' @importFrom geometry delaunayn
TIN = function(x,y,z)
{
  # Delaunay triangluation (renvoie le numero des points)
  p       <- matrix(c(x,y), ncol=2)
  tin     <- geometry::delaunayn(p)

  n = dim(tin)[1]

  # Retrieve point coordinates
  triangles = array( rep(NA, n*3*3), dim = c(n, 3, 3) )

  triangles[1:n, 1, 1] = x[tin[,1]]
  triangles[1:n, 2, 1] = x[tin[,2]]
  triangles[1:n, 3, 1] = x[tin[,3]]

  triangles[1:n, 1, 2] = y[tin[,1]]
  triangles[1:n, 2, 2] = y[tin[,2]]
  triangles[1:n, 3, 2] = y[tin[,3]]

  triangles[1:n, 1, 3] = z[tin[,1]]
  triangles[1:n, 2, 3] = z[tin[,2]]
  triangles[1:n, 3, 3] = z[tin[,3]]

  triangles = as.data.table(as.data.frame(triangles))
  setnames(triangles, c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3"))

  attr(triangles, "class") = c("TIN", attr(triangles, "class"))

  return(triangles)
}

#' Rasterize a triangular irregular network
#'
#' Rasterize a triangular irregular network
#' @param tin An object of class \code{TIN}
#' @param res scalar. the resolution of the rasterization
#' @return A data.table containing the x, y and z coordinates of the cells
#' @examples
#' x = runif(20)
#' y = runif(20)
#' z = runif(20)
#' tin = TIN(x, y, z)
#' rasterizeTIN(tin, 0.05)
#' @note Still in developpment. Currently very slow.
#' @seealso
#' \link[lidR:TIN]{TIN}
#' @export rasterizeTIN
rasterizeTIN <- function(tin, res)
{
  minx = min(tin$x1, tin$x2, tin$x3)
  maxx = max(tin$x1, tin$x2, tin$x3)
  miny = min(tin$y1, tin$y2, tin$y3)
  maxy = max(tin$y1, tin$y2, tin$y3)

  x = seq(minx, maxx, res)
  y = seq(miny, maxy, res)

  gri = expand.grid(x,y)
  names(gri) = c("x", "y")
  gri %<>% as.data.table

  cat("\nTIN interpolation :", dim(tin)[1],"\n")

  p <- dplyr::progress_estimated(dim(tin)[1])

  ret <- apply(tin, 1,
      function(x)
      {
        p$tick()$print()
        A = c(x[1], x[4], x[7])
        B = c(x[2], x[5], x[8])
        C = c(x[3], x[6], x[9])

        .triangle_interpolation(A,B,C, gri)
      })

  ret <- do.call(rbind, ret)

  setnames(ret, c("X", "Y", "Z"))

  ret %<>% unique(by=c("X", "Y"))

  return(ret)
}

#' Plot triangular irregular network TIN
#'
#' This functions implements a \link[graphics:plot]{plot} method for class TIN
#'
#' @param x An object of class \code{TIN}
#' @param ... Unused
#' @examples
#' x = runif(20)
#' y = runif(20)
#' z = runif(20)
#' tin = TIN(x, y, z)
#' plot(tin)
#' @seealso
#' \link[lidR:TIN]{TIN}
#' @rdname plot
#' @export plot.TIN
#' @method plot TIN
plot.TIN = function(x, ...)
{
  minx = min(x$x1, x$x2, x$x3)
  maxx = max(x$x1, x$x2, x$x3)
  miny = min(x$y1, x$y2, x$y3)
  maxy = max(x$y1, x$y2, x$y3)

  plot(minx,miny,xlim=c(minx,maxx), ylim=c(miny,maxy), col="white", asp=1, xlab="X", ylab="Y")

  invisible( apply(x, 1, function(x) { lines(c(x[1], x[2], x[3], x[1]), c(x[4], x[5], x[6], x[4])) } ))
}

.triangle_interpolation = function(A, B, C, points)
{
    tripoints = points[sp::point.in.polygon(points$x, points$y, c(A[1], B[1], C[1]), c(A[2], B[2], C[2])) > 0]

    pl  = .plane_param(A,B,C)
    z   = .eval_plane(tripoints$x, tripoints$y, pl)

    return(data.table(x = tripoints$x,y =tripoints$y,z))
}

.plane_param = function(A,B,C)
{
  AB = A-B
  AC = A-C
  n = .crossprod3d(AB, AC)

  d = n[1]*A[1] + n[2]*A[2] + n[3]*A[3]

  return(c(n[1], n[2],n[3], d))
}

.crossprod3d = function(u,v)
{
  w    = numeric(3)
  w[1] = u[2]*v[3] - u[3]*v[2]
  w[2] = u[3]*v[1] - u[1]*v[3]
  w[3] = u[1]*v[2] - u[2]*v[1]
  return(w)
}

.eval_plane = function(x, y, p)
{
  z = -(p[1]*x + p[2]*y - p[4])/p[3]
  return(z)
}

