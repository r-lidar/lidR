raster_template <- function(raster)
{
  if (is(raster, "raster_template"))
    return(raster)

  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
  {
    bbox <- raster_bbox(raster)
    xmin <- bbox$xmin
    xmax <- bbox$xmax
    ymin <- bbox$ymin
    ymax <- bbox$ymax
    nrow <- nrow(raster)
    ncol <- ncol(raster)
    xres <- round((xmax - xmin)/ncol, 5)
    yres <- round((ymax - ymin)/nrow, 5)
    crs  <- sf::st_crs(bbox)
  }
  else if (is(raster, "stars"))
  {
    sdim   <- stars::st_dimensions(raster)
    stattr <- attr(stars::st_dimensions(raster), "raster")

    # https://r-spatial.github.io/stars/articles/stars4.html
    if (sdim$y$delta > 0) stop("stars objects with delta > 0 are not yet supported", call. = FALSE)
    if (!all(stattr$affine == 0)) stop("Rotated and sheared stars objects are not yet supported", call. = FALSE)
    if (stattr$curvilinear == TRUE) stop("Curvilinear stars objects are not yet supported", call. = FALSE)

    bbox <- raster_bbox(raster)
    xmin <- bbox$xmin
    xmax <- bbox$xmax
    ymin <- bbox$ymin
    ymax <- bbox$ymax
    xres <- sdim$x$delta
    yres <- abs(sdim$y$delta)
    ncol <- as.integer(round((xmax-xmin)/xres))
    nrow <- as.integer(round((ymax-ymin)/yres))
    crs  <- raster_crs(raster)
  }
  else
  {
    raster_error() # nocov
  }

  layout <- list(xmin = xmin, xmax = xmax,
                 ymin = ymin, ymax = ymax,
                 ncol = ncol, nrow = nrow,
                 xres = xres, yres = yres,
                 crs  = crs)

  class(layout) <- "raster_template"
  return(layout)
}

raster_cell_from_xy = function(raster, x, y)
{
  e    <- raster_template(raster)
  xmin <- e$xmin
  xmax <- e$xmax
  ymin <- e$ymin
  ymax <- e$ymax
  xres <- e$xres
  yres <- e$yres
  ncol <- e$ncol
  nrow <- e$nrow

  col  <- floor((x - xmin) / xres);
  col[x == xmax] <- ncol-1;
  col[col >= ncol | col < 0L] <- NA_integer_

  row  <- floor((ymax - y) / yres);
  row[y == ymin] <- nrow-1;
  row[row >= nrow | row < 0L] <- NA_integer_

  cell <- as.integer(row * ncol + col + 1)
  return(cell)
}

raster_value_from_xy = function(raster, x, y, layer = 1)
{
  if (is(raster, "stars_proxy"))
  {
    xrange <- range(x)
    yrange <- range(y)
    bbox   <- sf::st_bbox(c(xmin = xrange[1], xmax = xrange[2], ymin = yrange[1], ymax = yrange[2]), crs = raster_crs(raster))
    raster <- raster[bbox]
    raster <- stars::st_as_stars(raster)
  }

  cells <- raster_cell_from_xy(raster, x, y)
  return(raster_value_from_cells(raster, cells, layer))
}

raster_value_from_cells = function(raster, cells, layer = 1)
{
  if (inherits(raster, "Raster"))
  {
    if (raster_nlayer(raster) > 1)
      return(raster::extract(raster, cells, layer = layer, nl = 1)[,1])
    else
      return(raster::extract(raster, cells, layer = layer, nl = 1))
  }

  if (is(raster, "stars_proxy"))
    stop("stars_proxy not yet supported in 'raster_value_from_cells()'", call. = FALSE) # nocov

  if (is(raster, "stars"))
  {
    dims <- dim(raster)

    if (is.na(dims[3]))
      return(raster[[1]][cells])

    return(raster[,,,layer][[1]][cells])
  }

  if (is(raster, "SpatRaster"))
    return(terra::extract(raster, cells)[[layer]])

  raster_error() # nocov
}

raster_crop <- function(raster, bbox)
{
  if (inherits(raster, "Raster"))
  {
    bbox <- raster::extent(bbox)
    return(raster::crop(raster, bbox))
  }

  if (is(raster, "stars"))
  {
    # Workaround for https://github.com/r-spatial/stars/issues/463
    rbbox <- sf::st_bbox(raster)
    xmin  <- max(rbbox$xmin, bbox$xmin)
    ymin  <- max(rbbox$ymin, bbox$ymin)
    xmax  <- min(rbbox$xmax, bbox$xmax)
    ymax  <- min(rbbox$ymax, bbox$ymax)
    bbox  <- sf::st_bbox(c(xmin = xmin , ymin = ymin, xmax = xmax, ymax = ymax), crs = sf::st_crs(bbox))
    return(sf::st_crop(raster, bbox))
  }

  if (is(raster, "SpatRaster"))
  {
    bbox <- as.numeric(bbox)
    bbox <- bbox[c(1,3,2,4)]
    bbox <- terra::ext(bbox)
    return(terra::crop(raster, bbox))
  }

  raster_error() # nocov
}

raster_as_matrix <- function(raster, downsample = FALSE)
{
  size <- 1000*1000*10

  if (raster_is_proxy(raster))
  {
    if (raster_ncell(raster) > size)
    {
      if (downsample == FALSE)
        stop("Cannot convert this on-disk raster into a matrix without downsampling")

      raster <- raster_downsample(raster, size)
    }
    else
    {
      if (is(raster, "stars"))
        raster <- stars::st_as_stars(raster)
    }
  }

  if (is(raster, "RasterLayer"))
  {
    mx <-  t(apply(raster::as.matrix(raster), 2, rev))
    x  <- sort(raster::xFromCol(raster, 1:raster::ncol(raster)))
    y  <- sort(raster::yFromRow(raster, 1:raster::nrow(raster)))
    return(list(x = x, y = y, z = mx))
  }

  if (is(raster, "stars"))
  {
    sdim   <- stars::st_dimensions(raster)
    if (sdim$y$delta > 0) stop("stars objects with delta > 0 are not yet supported", call. = FALSE)

    dims <- dim(raster)

    if (is.na(dims[3]))
      mx <-  raster[[1]]
    else
      mx <- raster[,,,1][[1]]

    x <- stars::st_get_dimension_values(raster, 'x')
    y <- stars::st_get_dimension_values(raster, 'y')
    mx <- t(apply(mx, 1, rev))
    return(list(x = x, y = rev(y), z = mx))
  }

  if (is(raster, "SpatRaster"))
  {
    mx <- t(apply(terra::as.matrix(raster, wide = TRUE), 2, rev))
    x  <- sort(terra::xFromCol(raster, 1:terra::ncol(raster)))
    y  <- sort(terra::yFromRow(raster, 1:terra::nrow(raster)))
    return(list(x = x, y = y, z = mx))
  }

  raster_error() # nocov
}

#' @importFrom stats na.omit
raster_as_dataframe <- function(raster,  xy = TRUE, na.rm = TRUE)
{
  ondisk <- raster_is_proxy(raster)

  # Small rasters can be loaded on the fly
  if (ondisk & raster_fits_in_memory(raster, n = 10))
  {
    raster <- raster_in_memory(raster)
    ondisk <- FALSE
  }

  if (ondisk)
    stop("On-disk rasters not supported in 'raster_as_dataframe()'", call. = FALSE) # nocov

  m <- raster_as_matrix(raster)
  z <- as.numeric(t(apply(m$z, 1, rev)))
  x <- rep(m$x, length(m$y))
  y <- rep(rev(m$y), each = length(m$x))
  d <- data.frame(X = x, Y = y)

  if (!xy) d$Z = z

  data.table::setDT(d)

  if (na.rm)
  {
    nas <- is.na(z)
    d <- d[!nas]
  }

  return(d)
}

raster_as_las <- function(raster, bbox = NULL)
{
  ondisk <- raster_is_proxy(raster)

  # Small rasters can be loaded on the fly
  if (ondisk & raster_fits_in_memory(raster, n = 10))
  {
    raster <- raster_in_memory(raster)
    ondisk <- FALSE
  }

  if (ondisk & is.null(bbox))
    stop("On-disk rasters not supported without a bbox in 'raster_as_las()'", call. = FALSE) # nocov

  if (ondisk & !is.null(bbox))
    raster <- raster_crop(raster, bbox)

  data <- raster_as_dataframe(raster, xy = FALSE, na.rm = TRUE)
  header <- rlas::header_create(data)
  las <- LAS(data, header, crs = raster_crs(raster), check = FALSE)
  return(las)
}

raster_res <- function(raster)
{
  res <- raster_template(raster)
  return(unname(c(res$xres, res$yres)))
}

raster_ncell <- function(raster)
{
  res <- raster_template(raster)
  return(unname(res$nrow * res$ncol))
}

raster_names <- function(raster)
{
  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
    return(names(raster))

  if (is(raster, "stars"))
  {
    dims <- dim(raster)

    if (is.na(dims[3]))
      return(names(raster))
    else
      return(stars::st_get_dimension_values(raster, 3))
  }

  raster_error() # nocov
}

`raster_names<-` <- function(raster, value)
{
  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
  {
    names(raster) <- value
    return(raster)
  }

  if (is(raster, "stars"))
  {
    dims <- dim(raster)

    if (is.na(dims[3]))
      names(raster) <- value
    else
      raster <- stars::st_set_dimensions(raster, 3, values = value)

    return(raster)
  }

  raster_error() # nocov
}

raster_is_supported <- function(raster)
{
  return(inherits(raster, "Raster") | is(raster, "stars") | is(raster, "SpatRaster") | is(raster, "raster_template"))
}

raster_build_vrt = function(file_list, vrt)
{
  file_list <- unlist(file_list)
  layers    <- names(terra::rast(file_list[1]))
  folder    <- dirname(file_list[1])
  file      <- paste0("/", vrt, ".vrt")
  vrt       <- paste0(folder, file)
  sf::gdal_utils("buildvrt", source = file_list, destination = vrt, quiet = TRUE)
  return(vrt)
}

raster_layout <- function(las, res, start = c(0,0), buffer = 0, format = "template")
{
  if (is_raster(res))
  {
    if (raster_nlayer(res) > 1)
      stop("Multilayer rasters are not supported as a template", call. = FALSE)

    resolution <- raster_res(res)
    if (resolution[1] != resolution[2])
      stop("Rasters with different x y resolutions are not supported as a template", call. = FALSE)

    pkg <- raster_pkg(res)
    res <- raster_template(res)
    res <- raster_materialize(res, pkg, NA_real_)
    return(res)
  }

  assert_all_are_non_negative(res)
  bbox <- st_adjust_bbox(las, res, start, buffer)

  if (format == "stars")
  {
    layout  <- stars::st_as_stars(bbox, dx = res, value = NA_real_)
    return(layout)
  }

  if (format == "raster")
  {
    bbox   <- raster::extent(bbox)
    layout <- suppressWarnings(raster::raster(bbox, res = res, crs = as(st_crs(las), "CRS")))
    suppressWarnings(layout[] <- NA_real_)
    raster::crs(layout) <- raster::crs(las)
    return(layout)
  }

  if (format == "terra")
  {
    crs    <- st_crs(las)$wkt
    bbox   <- as.numeric(bbox)
    bbox   <- bbox[c(1,3,2,4)]
    bbox   <- terra::ext(bbox)
    layout <- terra::rast(bbox)
    terra::res(layout) <- res
    suppressWarnings(layout[] <- NA_real_)
    terra::crs(layout) <- crs
    return(layout)
  }

  xmin <- bbox$xmin
  xmax <- bbox$xmax
  ymin <- bbox$ymin
  ymax <- bbox$ymax
  xres <- res
  yres <- res
  ncol <- round((xmax - xmin)/xres)
  nrow <- round((ymax - ymin)/yres)

  layout <- list(xmin = xmin, xmax = xmax,
                 ymin = ymin, ymax = ymax,
                 ncol = ncol, nrow = nrow,
                 xres = xres, yres = yres,
                 crs  = st_crs(las))

  class(layout) <- "raster_template"
  return(layout)
}

raster_materialize <- function(raster, pkg = getOption("lidR.raster.default"), values = NA_real_)
{
  if (!is(raster, "raster_template"))
    return(raster)

  bbox <- raster_bbox(raster)

  if (pkg == "raster")
  {
    crs  <- as(st_crs(bbox), "CRS")
    bbox <- as.numeric(bbox)
    bbox <- bbox[c(1,3,2,4)]
    bbox <- raster::extent(bbox)
    res  <- as.numeric(raster$xres)
    out  <- suppressWarnings(raster::raster(bbox, res = res, crs = crs))
    suppressWarnings(out[] <- values)
    return(out)
  }

  if (pkg == "stars")
  {
    res  <- stars::st_as_stars(bbox, nx = raster$ncol, ny = raster$nrow, values = values)
    return(res)
  }

  if (pkg == "terra")
  {
    crs  <- sf::st_crs(bbox)$wkt
    bbox <- as.numeric(bbox)
    bbox <- bbox[c(1,3,2,4)]
    bbox <- terra::ext(bbox)
    res  <- terra::rast(bbox)
    terra::res(res) <- raster$xres
    suppressWarnings(res[] <- values)
    terra::crs(res) <- crs
    return(res)
  }

  raster_error() # nocov
}

raster_alignment <- function(res, start = c(0, 0))
{
  if (is_raster(res))
  {
    ext       <- raster_bbox(res)
    r         <- round(raster_res(res)[1], 5)
    start     <- round(c(ext$xmin, ext$ymin), 5)
    alignment <- list(res = r, start = start)
    return(alignment)
  }

  alignment   <- list(res = res, start = start)
  return(alignment)
}

raster_pkg <- function(raster)
{
  if (inherits(raster, "Raster"))
    return("raster")

  if (is(raster, "stars"))
    return("stars")

  if (is(raster, "SpatRaster"))
    return("terra")

  raster_error() # nocov
}

raster_nlayer <- function(raster)
{
  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
  {
    return(dim(raster)[3])
  }

  if (is(raster, "stars"))
  {
    dims <- dim(raster)
    if (is.na(dims[3])) return(1L)
    return(dims[3])
  }

  raster_error() # nocov
}

raster_set_values <- function(raster, values, cells = NULL)
{
  if (is(raster, "raster_template"))
    stop("Internal error in raster_value: the raster must be materialized. Please report.", call. = FALSE) # nocov

  if (raster_nlayer(raster) > 1)
    stop("Internal error in raster_values: the raster must be a single layer. Please report.", call. = FALSE) # nocov

  if (is.null(cells))
    cells <- 1:raster_ncell(raster)

  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
  {
    suppressWarnings(raster[cells] <- values)
    return(raster)
  }

  if (is(raster, "stars"))
  {
    raster[[1]][cells] <- values
    storage.mode(raster[[1]]) <- storage.mode(values)
    return(raster)
  }

  raster_error() # nocov
}

raster_values <- function(raster)
{
  if (is(raster, "raster_template"))
    stop("Internal error in raster_value: the raster must be materialized. Please report.", call. = FALSE) # nocov

  if (raster_nlayer(raster) > 1)
    stop("Internal error in raster_values: the raster must a single layer. Please report.", call. = FALSE) # nocov

  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
    return(raster[])

  if (is(raster, "stars"))
    return(as.numeric(raster[[1]][]))

  raster_error() # nocov
}

raster_replace_na <- function(raster, value = 0)
{
  if (is(raster, "raster_template"))
    stop("Internal error in replace_na: the raster must be materialized. Please report.", call. = FALSE) # nocov

  if (inherits(raster, "Raster") | is(raster, "SpatRaster"))
  {
    raster[is.na(raster)] <- value
    return(raster)
  }

  if (is(raster, "stars"))
  {
    raster[[1]][is.na(raster[[1]])] <- value
    return(raster)
  }

  raster_error() # nocov
}

raster_error <- function()
{
  parent = deparse(sys.calls()[[sys.nframe()-1]])
  stop(glue::glue("Internal error in {parent}: raster must be a raster from raster, stars,7 or terra. Please report"), call. = FALSE) # nocov
}

raster_is_proxy <- function(raster)
{
  if (is(raster, "stars_proxy"))
    return(TRUE)

  if (inherits(raster, "Raster"))
    return(!raster::inMemory(raster))

  if (is(raster,"SpatRaster"))
    return(!terra::inMemory(raster))

  return(FALSE)
}

raster_downsample <- function(raster, size)
{
  cat("downsample to", size, "cells\n")

  if (raster_ncell(raster) < size)
    return(raster)

  if (is(raster, "stars"))
  {
    downsample <- floor(1/sqrt(size/raster_ncell(raster)))
    raster <- stars::st_as_stars(raster, downsample = downsample)
    return(raster)
  }

  if (inherits(raster, "Raster"))
  {
    raster = raster::sampleRegular(raster, size, asRaster = TRUE, useGDAL = TRUE)
    return(raster)
  }

  if (is(raster, "SpatRaster"))
  {
    raster = terra::spatSample(raster, size, method = "regular", as.raster = TRUE)
    return(raster)
  }

  raster_error() # nocov
}

raster_bbox <- function(raster)
{
  if (is(raster, "raster_template"))
    return(sf::st_bbox(raster))

  if (is(raster, "stars"))
    return(sf::st_bbox(raster))

  if (inherits(raster, "Raster"))
    return(sf::st_bbox(raster))

  if (is(raster, "SpatRaster"))
  {
    # sf::st_bbox now works properly on SpatRaster.
    return(sf::st_bbox(raster))
  }

  raster_error() # nocov
}

raster_crs <- function(raster)
{
  if (is(raster, "SpatRaster"))
  {
    if (terra::crs(raster) == "")
      return(sf::NA_crs_)
  }

  return(sf::st_crs(raster))
}

raster_pkg <- function(raster)
{
  if (is(raster, "stars"))
    return("stars")

  if (inherits(raster, "Raster"))
    return("raster")

  if (is(raster,"SpatRaster"))
    return("terra")

  raster_error()
}

raster_size <- function(raster)
{
  if (is(raster, "stars"))
  {
    if (raster_nlayer(raster) == 1)
      return(c(dim(raster), 1))
    else
      return(dim(raster))
  }

  if (inherits(raster, "Raster") | is(raster,"SpatRaster"))
    return(dim(raster))

  raster_error()
}

raster_multilayer_class <- function(pkg = getOption("lidR.raster.default"))
{
  if (pkg == "raster") return("RasterBrick")
  if (pkg == "terra") return("SpatRaster")
  if (pkg == "stars") return("stars")
}

raster_class <- function(pkg = getOption("lidR.raster.default"))
{
  if (pkg == "raster") return("RasterLayer")
  if (pkg == "terra") return("SpatRaster")
  if (pkg == "stars") return("stars")
}

raster_fits_in_memory <- function(raster, n = 1)
{
  if (!raster_is_proxy(raster)) return(TRUE)

  nc <- raster_ncell(raster)
  n <- n * raster_nlayer(raster)
  memneed <- nc * n * 8L
  memavail <- terra::free_RAM()*1000
  memavail <- 0.6 * memavail
  return(memneed < memavail)
}

raster_in_memory <- function(raster)
{
  if (is(raster, "stars"))
    return(stars::st_as_stars(raster))

  if (inherits(raster, "Raster"))
    return(raster::readAll(raster))

  if (is(raster,"SpatRaster"))
    return(raster*1)

  raster_error()
}


# Workaround for #580 #622. If normalize height is ran in parallel it will fail with
# SpatRaster because they are not serializable. SpatRaster are converted to RasterLayer
# for multicore strategies
convert_ondisk_spatraster_into_serializable_raster_if_necessary <- function(x)
{
  if (is_raster(x) && raster_pkg(x) == "terra")
  {
    ncores <- try_to_get_num_future_cores()
    if (!is.null(ncores) && ncores >= 2L)
      x <- raster::raster(x)
  }

  return(x)
}
