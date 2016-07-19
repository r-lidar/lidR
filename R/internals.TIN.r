#' @importFrom data.table data.table
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

