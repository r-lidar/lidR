grid_catalog <- function(ctg, grid_func, res, filter, buffer, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- NULL

  param = list(...)

  # Tweak to enable non-standard evaluation of func in grid_metrics
  if (!is.null(param$func))
  {
    if (is.call(param$func))
      param$func = as.expression(param$func)
  }

  X = make_cluster(ctg, res, buffer)
  X = apply(X, 1, as.list)

  if (LIDROPTIONS("progress"))
    p = utils::txtProgressBar(max = length(X), style = 3)
  else
    p = NULL

  mc.cores = LIDROPTIONS("multicore")

  if(mc.cores == 1)
  {
    verbose("Computing sequentially the metrics for each cluster...")

    output = lapply(X, .getMetrics, grid_func = grid_func, ctg = ctg, res = res, filter = filter, param = param, p = p)
  }
  else
  {
    verbose("Computing sequentially (multicore) the metrics for each cluster...")

    cl = parallel::makeCluster(mc.cores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, X, fun = .getMetrics, grid_func = grid_func, ctg = ctg, res = res, filter = filter, param = param)
    parallel::stopCluster(cl)
  }

  ._class = class(output[[1]])

  output = data.table::rbindlist(output)
  data.table::setattr(output, "class", ._class)

  return(output)
}

.getMetrics = function(X, grid_func, ctg, res, filter, param, p = NULL)
{
  Y <- NULL

  xleft   = X$xleft
  xright  = X$xright
  ybottom = X$ybottom
  ytop    = X$ytop
  name    = paste0("ROI", X$name)

  x = (xleft + xright)/2
  y = (ybottom + ytop)/2
  r = (X$xrightbuff - X$xleftbuff)/2


  las = catalog_queries(ctg, x, y, r, r, name, filter, disable_bar = T, nomulticore = T)[[1]]

  # Skip if the ROI fall in a void area
  if (is.null(las)) return(NULL)

  # Because catalog_queries keep point inside the boundingbox (close interval) but point which
  # are exactly on the boundaries are counted twice. Here a post-process to make an open
  # interval on left and bottom edge of the boudingbox.
  las = lasfilter(las, X > xleft, Y > ybottom)

  # Very unprobable but who knows...
  if (is.null(las)) return(NULL)

  param$x = las
  param$res  = res

  m = do.call(grid_func, args = param)
  m = m[X >= xleft & X <= xright & Y >= ybottom & Y <= ytop]  # remove the buffer

  if (!is.null(p))
  {
    i = utils::getTxtProgressBar(p) + 1
    utils::setTxtProgressBar(p, i)
  }

  return(m)
}

make_cluster = function(ctg, res, buffer = 0)
{
  # Will process subtiles of 500 by 500 m
  size = 500

  # dimension of the clusters (width = height)
  width = ceiling(size/res) * res

  #verbose("Computing the bounding box of the catalog...")

  # Bounding box of the catalog
  bbox = ctg %$% c(min(Min.X), min(Min.Y), max(Max.X), max(Max.Y))

  # Buffer around the bbox as a multiple of the resolution
  buffered_bbox = bbox + c(-res, -res, +res, +res)
  buffered_bbox = round_any(buffered_bbox, res)
  buffered_bbox = buffered_bbox + c(-res, -res, +res, +res)

  #verbose("Creating a set of cluster for the catalog...")

  # Generate coordinates of sub bounding boxes
  xleft   = seq(buffered_bbox[1], buffered_bbox[3], width)
  ybottom = seq(buffered_bbox[2], buffered_bbox[4], width)

  X = expand.grid(xleft = xleft, ybottom = ybottom)

  X$xright = X$xleft + width
  X$ytop   = X$ybottom + width

  X$xleftbuff   = X$xleft - buffer
  X$ybottombuff = X$ybottom - buffer
  X$xrightbuff  = X$xright + buffer
  X$ytopbuff    = X$ytop + buffer

  X$name = 1:nrow(X)

  # Plot the pattern
  xrange = c(min(X$xleft), max(X$xright))
  yrange = c(min(X$ybottom), max(X$ytop))
  title  = "Pattern of clusters"
  graphics::plot(ctg, main = title, xlim = xrange, ylim = yrange)
  X %$% graphics::rect(xleft, ybottom, xright, ytop, border = "red")

  if (buffer > 0)
    X %$% graphics::rect(xleftbuff, ybottombuff, xrightbuff, ytopbuff, border = "darkgreen", lty = "dotted")

  return(X)
}


