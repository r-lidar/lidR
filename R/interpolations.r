interpolate = function(points, coord, method, k, model)
{
  if(method == "knnidw")
  {
    cat("[using inverse distance weighting]\n")
    return(interpolate_knnidw(points, coord, k))
  }
  else if(method == "delaunay")
  {
    cat("[using Delaunay triangulation]\n")
    return(interpolate_delaunay(points, coord))
  }
  else if(method == "kriging")
  {
    return(interpolate_kriging(points, coord, model, k))
  }
  else
    stop(paste0("Method '", method, "' does not exist."), call. = FALSE)
}

interpolate_knnidw = function(ground, coord, k)
{
  . <- X <- Y <- NULL

  nn = RANN::nn2(ground[, .(X,Y)], coord[, .(X,Y)], k = k)
  idx = nn$nn.idx
  w = 1/nn$nn.dist
  w = ifelse(is.infinite(w), 1e8, w)
  z = matrix(ground[as.numeric(idx)]$Z, ncol = dim(w)[2])

  return(rowSums(z*w)/rowSums(w))
}

interpolate_delaunay = function(ground, coord)
{
  . <- X <- Y <- Z <- Zg <- xc <- yc <- NULL

  if (!requireNamespace("akima", quietly = TRUE))
    stop("'akima' package is needed for this function to work. Please install it.", call. = F)

  xo = unique(coord$X) %>% sort()
  yo = unique(coord$Y) %>% sort()

  grid = ground %$% akima::interp(X, Y, Z, xo = xo, yo = yo, duplicate = "user", dupfun = min)

  temp = data.table::data.table(xc = match(coord$X, grid$x), yc = match(coord$Y, grid$y))
  temp[, Zg := grid$z[xc,yc], by = .(xc,yc)]

  return(temp$Zg)
}

interpolate_kriging = function(ground, coord, model, k)
{
  X <- Y <- Z <- NULL

  if (!requireNamespace("gstat", quietly = TRUE))
    stop("'gstat' package is needed for this function to work. Please install it.", call. = F)

  x  = gstat::krige(Z~X+Y, location = ~X+Y, data = ground, newdata = coord, model, nmax = k)
  return(x$var1.pred)
}

# interpolate_delaunay = function(ground, coord)
# {
#   # Computes Delaunay triangulation
#   triangles <-  deldir::deldir(ground$X, ground$Y) %>%  deldir::triang.list()
#
#   # Comptutes equation of planes
#   eq = lapply(triangles, function(x)
#   {
#     x = data.table::data.table(x)
#     x[, z := ground$Z[ptNum]][, ptNum := NULL]
#
#     u = x[1] - x[2]
#     v = x[1] - x[3]
#
#     n = c(u$y*v$z-u$z*v$y, u$z*v$x-u$x*v$z, u$x*v$y-u$y*v$x)
#     n[4] = sum(-n*x[3])
#
#     return(n)
#   })
#
#   eq = do.call(rbind, eq)
#
#   xcoords = lapply(triangles, function(x){ x$x })
#   ycoords = lapply(triangles, function(x){ x$y })
#
#   ids = lidR:::points_in_polygons(xcoords, ycoords, coord$X, coord$Y)
#
#   z = rep(NA, dim(coord)[1])
#   z[ids > 0] = -(coord$X[ids > 0]*eq[ids,1] + coord$Y[ids > 0]*eq[ids,2]+eq[ids,4])/eq[ids,3]
#
#   return(z)
# }