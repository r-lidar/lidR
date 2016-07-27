#' Triangular irregular network
#'
#' Triangular irregular network computed from a Delaunay triangulation
#' @param x numeric. n points x coordinates
#' @param y numeric. n points y coordinates
#' @param z numeric. n points z coordinates
#' @return A data.table containig the list of triangles coordinates in each row
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
#' @importFrom data.table setnames as.data.table
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