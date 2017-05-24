grid_catalog <- function(ctg, grid_func, res, filter, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- NULL

  # Will process subtiles of 500 by 500 m
  size = 500
  R = ceiling(size/res) * res

  param = lazyeval::dots_capture(...)

  # Tweak to enable non-standard evaluation of func in grid_metrics
  if (!is.null(param$func))
  {
    param$func = lazyeval::uq(param$func)

    if (is.call(param$func))
      param$func = as.expression(param$func)
  }

  verbose("Computing the bounding box of the catalog...")

  # Bounding box of the catalog
  bbox = ctg %$% c(min(Min.X), min(Min.Y), max(Max.X), max(Max.Y))

  # Buffer around the bbox
  buffered_bbox = bbox + c(-res, -res, +res, +res)
  buffered_bbox = round_any(buffered_bbox, res)
  buffered_bbox = buffered_bbox + c(-0.5*res, -0.5*res, +0.5*res, +0.5*res)

  verbose("Creating a set of cluster for the catalog...")

  # Generate coordinates of sub bounding boxes
  x = seq(buffered_bbox[1]-0.5*res, buffered_bbox[3]+0.5*res, R)
  y = seq(buffered_bbox[2]-0.5*res, buffered_bbox[4]+0.5*res, R)
  X = expand.grid(x = x, y = y)
  X$name = 1:nrow(X)

  # Plot the pattern
  xrange = c(min(X$x), max(X$x) + R)
  yrange = c(min(X$y), max(X$y) + R)
  title  = "Pattern of clusters"
  graphics::plot(ctg, main = title, xlim = xrange, ylim = yrange)
  graphics::rect(X$x, X$y, X$x+R, X$y+R, border = "red")

  # Convert coordinates as a list for lapply and parLapply
  X = apply(X, 1, as.list)

  if (LIDROPTIONS("progress"))
    p = utils::txtProgressBar(max = length(X), style = 3)
  else
    p = NULL

  mc.cores = LIDROPTIONS("multicore")

  if(mc.cores == 1)
  {
    verbose("Computing sequentially the metrics for each cluster...")

    output = lapply(X, .getMetrics, grid_func = grid_func, ctg = ctg, res = res, R = R, filter = filter, param = param, p = p)
  }
  else
  {
    verbose("Computing sequentially (multicore) the metrics for each cluster...")

    cl = parallel::makeCluster(mc.cores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, X, fun = .getMetrics, grid_func = grid_func, ctg = ctg, res = res, R = R, filter = filter, param = param)
    parallel::stopCluster(cl)
  }

  ._class = class(output[[1]])

  output = data.table::rbindlist(output)
  data.table::setattr(output, "class", ._class)

  return(output)
}

.getMetrics = function(X, grid_func, ctg, res, R, filter, param, p = NULL)
{
  Y <- X <- NULL

  xleft   = X$x
  ybottom = X$y
  name    = paste0("ROI", X$name)
  halfR   = 0.5*R

  x = xleft   + halfR
  y = ybottom + halfR

  las = catalog_queries(ctg, x, y, halfR, halfR, name, filter, disable_bar = T, nomulticore = T)[[1]]

  # Skip if the ROI fall in a void area
  if(is.null(las)) return(NULL)

  # Because catalog_queries keep point inside the boundingbox (close interval) but point which
  # are exactly on the boundaries are counted twice. Here a post-process to make an open
  # interval on left and bottom edge of the boudingbox.
  las = lasfilter(las, X > xleft, Y > ybottom)

  # Very unprobable but who knows...
  if(is.null(las)) return(NULL)

  param$x = las
  param$res  = res

  m = do.call(grid_func, args = param)

  if (!is.null(p))
  {
    i = utils::getTxtProgressBar(p) + 1
    utils::setTxtProgressBar(p, i)
  }

  return(m)
}


