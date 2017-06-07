grid_catalog <- function(ctg, grid_func, res, filter, buffer, by_file, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- p <- NULL

  # Store some stuff in readable variables
  param = list(...)
  fname = lazyeval::expr_text(grid_func)
  dir   = paste0(dirname(ctg$filename[1]),  "/", fname, "/")
  cores = CATALOGOPTIONS("multicore")
  pbar  = LIDROPTIONS("progress")
  onhdd = CATALOGOPTIONS("return_virtual_raster")

  # Tweak to enable non-standard evaluation of func in grid_metrics-alike functions
  if (!is.null(param$func)) {
    if (is.call(param$func))
      param$func = as.expression(param$func)
  }

  # Test of memory to prevent memory overflow
  surface = sum(with(ctg, (Max.X - Min.X) * (Max.Y - Min.Y)))
  npixel  = surface / (res*res)
  nmetric = 3 # Must find a way to access this number
  nbytes  = npixel * nmetric * 8
  class(nbytes) = "object_size"

  if (nbytes > CATALOGOPTIONS("memory_limit_warning") & !onhdd)
  {
    size = format(nbytes, "auto")
    text = paste0("The process is expected to return an approximatly ", size, " object. It might be too much.\n")
    choices = c(
      "Proceed anyway",
      "Store the results on my disk an return a virtual raster mosaic",
      "Abort, let me configure myself with 'catalog_options()'")

    cat(text)
    choice = utils::menu(choices)

    if (choice == 2)
      onhdd = TRUE
    else if (choice == 3)
      return(invisible())
  }

  # Create a pattern of sub areas to be sequentially processed
  X = make_cluster(ctg, res, buffer, by_file)
  X = apply(X, 1, as.list)

  # Add the path to the saved file (if saved)
  X = lapply(X, function(x)
  {
    x$path = paste0(dir, fname, "_ROI", x$name, ".tiff")
    return(x)
  })

  # Enable progress bar
  if (pbar) p = utils::txtProgressBar(max = length(X), style = 3)

  # Create or clean the directory
  if (onhdd)
  {
    if (!dir.exists(dir))
      dir.create(dir)
    else
      unlink(dir, recursive = TRUE) ; dir.create(dir)
  }

  # Computations done within sequential or parallel loop in .getMetrics
  if (cores == 1)
  {
    verbose("Computing sequentially the metrics for each cluster...")
    output = lapply(X, .getMetrics, grid_func = grid_func, ctg = ctg, res = res, filter = filter, param = param, save_as_tiff = onhdd, p = p)
  }
  else
  {
    verbose("Computing in parallel the metrics for each cluster...")
    cl = parallel::makeCluster(cores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, X, fun = .getMetrics, grid_func = grid_func, ctg = ctg, res = res, filter = filter, param = param, save_as_tiff = onhdd, p = p)
    parallel::stopCluster(cl)
  }

  # Post process of the results (return adequate object)
  if (!onhdd)
  {
    ._class = class(output[[1]])
    output = data.table::rbindlist(output)
    data.table::setattr(output, "class", ._class)
  }
  else
  {
    # Build virtual raster mosaic and return it
    ras_lst = list.files(dir, full.names=T, pattern=".tif$")
    save_in = paste0(dir, "/", fname, ".vrt")
    gdalUtils::gdalbuildvrt(ras_lst, save_in)
    output  = raster::stack(save_in)
  }

  return(output)
}

# Apply for a given ROI of a catlog a grid_* function
#
# @param X list. the coordinates of the region of interest (rectangular)
# @param grid_func function. the grid_* function to be applied
# @param ctg  Catalog.
# @param res numric. the resolution to apply the grid_* function
# @param filter character. the streaming filter to be applied
# @param param list. the parameter of the function grid_function but res
# @param p progressbar.
.getMetrics = function(X, grid_func, ctg, res, filter, param, save_as_tiff, p)
{
  Y <- NULL

  # Convenient variables for readability
  xleft   = X$xleft
  xright  = X$xright
  ybottom = X$ybottom
  ytop    = X$ytop
  name    = paste0("ROI", X$name)
  path    = X$path
  xcenter = X$xcenter
  ycenter = X$ycenter
  width   = (X$xrightbuff - X$xleftbuff)/2

  # Extract the ROI as a LAS object
  las = catalog_queries(ctg, xcenter, ycenter, width, width, name, filter, disable_bar = T, no_multicore = T)[[1]]

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

  # Update progress bar
  if (!is.null(p))
  {
    i = utils::getTxtProgressBar(p) + 1
    utils::setTxtProgressBar(p, i)
  }

  # Return results or write file
  if (!save_as_tiff)
    return(m)
  else
  {
    if (nrow(m) == 0)
      return(NULL)

    m = as.raster(m)
    directory = dirname(path)
    raster::writeRaster(m, path, format = "GTiff")
    return(NULL)
  }
}

make_cluster = function(ctg, res, buffer, by_file)
{

  if (by_file)
  {
    X = ctg[, c("Min.X", "Max.X", "Min.Y", "Max.Y")]
    names(X) = c("xleft", "xright", "ybottom", "ytop")
  }
  else
  {
    # Will process subtiles of 1 km^2
    size = CATALOGOPTIONS("tiling_size")

    # dimension of the clusters (width = height)
    width = ceiling(size/res) * res

    verbose("Computing the bounding box of the catalog...")

    # Bounding box of the catalog
    bbox = with(ctg, c(min(Min.X), min(Min.Y), max(Max.X), max(Max.Y)))

    # Buffer around the bbox as a multiple of the resolution
    buffered_bbox = bbox + c(-res, -res, +res, +res)
    buffered_bbox = round_any(buffered_bbox, res)
    buffered_bbox = buffered_bbox + c(-res, -res, +res, +res)

    verbose("Creating a set of cluster for the catalog...")

    # Generate coordinates of sub bounding boxes
    xleft   = seq(buffered_bbox[1], buffered_bbox[3], width)
    ybottom = seq(buffered_bbox[2], buffered_bbox[4], width)

    X = expand.grid(xleft = xleft, ybottom = ybottom)

    X$xright = X$xleft + width
    X$ytop   = X$ybottom + width
  }

  X$xcenter     = (X$xleft + X$xright) / 2
  X$ycenter     = (X$ybottom + X$ytop) / 2
  X$xleftbuff   = X$xleft - buffer
  X$ybottombuff = X$ybottom - buffer
  X$xrightbuff  = X$xright + buffer
  X$ytopbuff    = X$ytop + buffer
  X$name        = 1:nrow(X)

  # Remove cluster outside the catalog
  index = suppressWarnings(catalog_index(ctg, X$xcenter, X$ycenter, width / 2, width / 2))
  keep  = sapply(index$tiles, length) > 0
  X = X[keep,]

  # Plot the pattern
  xrange = c(min(X$xleft), max(X$xright))
  yrange = c(min(X$ybottom), max(X$ytop))
  title  = "Pattern of clusters"
  graphics::plot(ctg, main = title, xlim = xrange, ylim = yrange)
  with(X, graphics::rect(xleft, ybottom, xright, ytop, border = "red"))

  if (buffer > 0)
    with(X, graphics::rect(xleftbuff, ybottombuff, xrightbuff, ytopbuff, border = "darkgreen", lty = "dotted"))

  return(X)
}

