lasaggregate = function(.las, by, call, res, start, colnames, splitlines, debug, name = "")
{
  . <- NULL

  if (is(call, "name"))
    call = eval(call)

  if (LIDROPTIONS("debug"))
  {
    output = with(.las@data, eval(call))
    .debug_metrics(output, call)
  }

  if (is(res, "RasterLayer"))
    by = "RASTER"

  if(by %in% c("XY", "XYZ", "HEXA"))
  {
    if(!is.numeric(res)) stop("Parameter 'res' should be numeric", call. = FALSE)
    if(res <= 0)         stop("Parameter 'res' should be greater than 0", call. = FALSE)
  }

  # Aggregation on XY (grid_metrics)
  if(by == "XY")
  {
    if(!is.numeric(start)) stop("Parameter 'start' should be numeric", call. = FALSE)
    if(2 != length(start)) stop("Parameter 'start' should have a length of 2", call. = FALSE)

    ._class = "lasmetrics"
    by = group_grid(.las@data$X, .las@data$Y, res, start)
  }
  # Aggregation on XYZ (grid_metrics3d)
  else if(by == "XYZ")
  {
    if(!is.numeric(start)) stop("Parameter 'start' should be numeric", call. = FALSE)
    if(3 != length(start)) stop("Parameter 'start' should have a length of 3", call. = FALSE)

    ._class = "lasmetrics3d"
    by = group_grid_3d(.las@data$X, .las@data$Y, .las@data$Z, res, start)
  }
  # Aggregation on hexagonal cells (grid_hexametrics)
  else if(by == "HEXA")
  {
    if (!requireNamespace("hexbin", quietly = TRUE))
      stop("'hexbin' package is needed for this function to work. Please install it.", call. = F)

    res = round(sqrt(((2*res*res)/(3*sqrt(3)))), 2)

    ext = extent(.las)
    xmin = round_any(ext@xmin, res)
    xmax = round_any(ext@xmax, res)
    ymin = round_any(ext@ymin, res)
    ymax = round_any(ext@ymax, res)

    if(xmax < ext@xmax) xmax = xmax + res
    if(xmin > ext@xmin) xmin = xmin - res
    if(ymax < ext@ymax) ymax = ymax + res
    if(ymin > ext@ymin) ymin = ymin - res

    dx = (xmax - xmin)
    dy = (ymax - ymin)

    xbins = (xmax - xmin)/(2*res)

    hbin_data  = hexbin::hexbin(.las@data$X, .las@data$Y, shape = dy/dx,  xbins = xbins, xbnds = c(xmin, xmax), IDs = TRUE)
    hbin_coord = hexbin::hcell2xy(hbin_data)
    hbin_ids   = hbin_data@cID
    hbin_pos   = cumsum(1:max(hbin_data@cell) %in% hbin_data@cell)
    hbin_pos_ids = hbin_pos[hbin_ids]

    ._class = "lashexametrics"
    by = list(Xr = hbin_coord$x[hbin_pos_ids], Yr = hbin_coord$y[hbin_pos_ids])
  }
  # Aggregation by trees (tree_metrics)
  else if (by == "TREE")
  {
    if(! name %in% names(.las@data))
      stop("The trees are not segmented yet. Please see function 'lastrees'.", call. = FALSE)

    ._class = NULL
    by = .las@data[, get(name)]
  }
  else if (by == "RASTER")
  {
    raster = res
    res = raster::res(raster)

    if (res[1] !=  res[2])
      stop("Rasters with different x y resolutions are not supported", call. = FALSE)

    res = res[1]
    cells = raster::cellFromXY(raster, .las@data[, .(X,Y), with = TRUE])
    values = suppressWarnings(raster[cells])
    X = raster::xFromCell(raster, cells)
    Y = raster::yFromCell(raster, cells)
    X[is.na(values)] = NA
    Y[is.na(values)] = NA

    by = list(Xgrid = X, Ygrid = Y)
    ._class = "lasmetrics"
  }

  # split flightlines option is alway possible but wrapper functions (the exported one) can
  # restrain possibilities
  if(splitlines & "flightlineID" %in% names(.las@data))
    by = c(by, list(flightline = .las@data$flightlineID))
  else if(splitlines & !"flightlineID" %in% names(.las@data))
    lidRError("LDR7")

  stat <- .las@data[, if (!anyNA(.BY)) c(eval(call)), by = by]

  n = names(stat)
  n[1:length(colnames)] = colnames

  data.table::setnames(stat, n)
  data.table::setattr(stat, "class", c(._class, attr(stat, "class")))
  data.table::setattr(stat, "res", res)

  return(stat)
}

.debug_metrics = function(metrics, func)
{
  funcstring = deparse(func)

  if(is.list(metrics) & !is.data.frame(metrics))
  {
    if(is.null(names(metrics)))
      names(metrics) = paste0("#", 1:length(metrics))

    classes = sapply(metrics, class)
    test = classes %in% c("integer", "numeric", "logical", "character")
    n = names(metrics[!test])
    c = classes[!test]

    if(sum(!test) == 1)
      lidRError("TFS1", expression = funcstring, metric = n, class = c)
    else if(sum(!test) > 1)
      lidRError("TFS2", expression = funcstring, metric = n, class = c)

    size = sapply(metrics, length)
    test = size == 1

    n = names(metrics[!test])
    c = size[!test]

    if(sum(!test) == 1)
      lidRError("TFS3", expression = funcstring, metric = n, number = c)
    else if(sum(!test) > 1)
      lidRError("TFS4", expression = funcstring, metric = n, number = c)
  }
  else if(is.data.frame(metrics))
    lidRError("TFS5", expression = funcstring)
  else if(is.vector(metrics) & length(metrics) > 1)
    lidRError("TFS6", expression = funcstring, number = length(metrics))
  else
    return(0)
}