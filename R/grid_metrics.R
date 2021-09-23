#' Area-Based Approach
#'
#' Computes a series of user-defined descriptive statistics for a LiDAR dataset within
#' each pixel of a raster (area-based approach). The grid cell coordinates are pre-determined for a
#' given resolution, so the algorithm will always provide the same coordinates independently of the
#' dataset. When start = (0,0) and res = 20 grid_metrics will produce the following cell centers:
#' (10,10), (10,30), (30,10) etc. aligning the corner of a cell on (0,0). When start = (-10, -10) and
#' res = 20 grid_metrics will produce the following cell centers: (0,0), (0,20), (20,0) etc. aligning
#' the corner of a cell on (-10, -10).
#'
#' @template param-las
#' @param func formula. An expression to be applied to each cell (see section "Parameter func").
#' @template param-res-grid
#' @param start vector of x and y coordinates for the reference raster. Default is (0,0) meaning that the
#' grid aligns on (0,0).
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See examples.
#' @param by_echo characters. The metrics are computed multiples times for different echo types. Can
#' be one or more of "all", "first", "intermediate", "lastofmany", "single", "multiple". See examples. Default
#' is "all" meaning that it computes metrics with all points provided.
#'
#' @section Parameter \code{func}:
#' The function to be applied to each cell is a classical function (see examples) that
#' returns a labeled list of metrics. For example, the following function \code{f} is correctly formed.
#' \preformatted{
#' f = function(x) {list(mean = mean(x), max = max(x))}
#' }
#' And could be applied either on the \code{Z} coordinates or on the intensities. These two
#' statements are valid:
#' \preformatted{
#' grid_metrics(las, ~f(Z), res = 20)
#' grid_metrics(las, ~f(Intensity), res = 20)
#' }
#' The following existing functions allow the user to
#' compute some predefined metrics:
#' \itemize{
#' \item{\link[=stdmetrics]{stdmetrics}}
#' \item{\link[=entropy]{entropy}}
#' \item{\link[=VCI]{VCI}}
#' \item{\link[=LAD]{LAD}}
#' }
#' But usually users must write their own functions to create metrics. \code{grid_metrics} will
#' dispatch the point cloud in the user's function.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-grid_functions
#'
#' @template return-grid-LayerBrick
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile)
#' col = height.colors(50)
#'
#' # === Using all points ===
#'
#' # Mean height with 400 m^2 cells
#' metrics = grid_metrics(las, ~mean(Z), 20)
#' plot(metrics, col = col)
#'
#' # Define your own new metrics
#' myMetrics = function(z, i) {
#'   metrics = list(
#'      zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#'      zimean  = mean(z*i),       # Mean products of z by intensity
#'      zsqmean = sqrt(mean(z^2))) # Quadratic mean
#'
#'    return(metrics)
#' }
#'
#' metrics = grid_metrics(las, ~myMetrics(Z, Intensity))
#'
#' plot(metrics, col = col)
#' #plot(metrics, "zwimean", col = col)
#' #plot(metrics, "zimean", col = col)
#'
#' # === With point filters ===
#'
#' # Compute using only some points: basic
#' first = filter_poi(las, ReturnNumber == 1)
#' metrics = grid_metrics(first, ~mean(Z), 20)
#'
#' # Compute using only some points: optimized
#' # faster and uses less memory. No intermediate object
#' metrics = grid_metrics(las, ~mean(Z), 20, filter = ~ReturnNumber == 1)
#'
#' # Compute using only some points: best
#' # ~50% faster and uses ~10x less memory
#' las = readLAS(LASfile, filter = "-keep_first")
#' metrics = grid_metrics(las, ~mean(Z), 20)
#'
#' # === Split by echo type ===
#' func = ~list(avgI = mean(Intensity))
#' echo = c("all", "first","multiple")
#' metrics <- grid_metrics(las, func , 20, by_echo = echo)
#' plot(metrics, col = heat.colors(25))
#' @family metrics
grid_metrics = function(las, func, res = 20, start = c(0,0), filter = NULL, by_echo = "all")
{
  UseMethod("grid_metrics", las)
}

#' @export
grid_metrics.LAS = function(las, func, res = 20, start = c(0,0), filter = NULL, by_echo = "all")
{
  # Defensive programming
  if (!is_a_number(res) & !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_numeric(start)
  formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!formula) func <- lazyeval::f_capture(func)
  echo_types <- c("all", "first", "intermediate", "lastofmany", "single", "multiple")
  stopifnot(all(by_echo %in% echo_types))

  # New 3.2.0 feature for computing metrics for each eho type
  all_echos   <- any(by_echo == "all")
  split_echos <- any(by_echo != "all")

  # Aggregation of the point cloud
  func   <- lazyeval::f_interp(func)
  call   <- lazyeval::as_call(func)
  layout <- rOverlay(las, res, start)
  cells  <- raster::cellFromXY(layout, coordinates(las))

  if (is(res, "RasterLayer") && all(is.na(cells)))
  {
    warning("No point fall in the raster. Bounding boxes are not intersecting.", call. = FALSE)
    return(layout)
  }

  if (all_echos)
    M1 <- grid_metrics_classic(las, layout, call, cells, filter)

  if (split_echos)
    M2 <- grid_metrics_by_echo_type(las, layout, call, cells, filter, by_echo)

  if (all_echos && split_echos)
    return(raster::brick(M1, M2))
  else if (all_echos)
    return(M1)
  else
    return(M2)
}

#' @export
grid_metrics.LAScluster = function(las, func, res = 20, start = c(0,0), filter = NULL, by_echo = "all")
{
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  bbox    <- raster::extent(las)
  metrics <- grid_metrics(x, func, res, start, filter, by_echo)
  metrics <- raster::crop(metrics, bbox)
  raster::crs(metrics) <- crs(x) # patch for raster not updated with rgal 1.5-8
  return(metrics)
}

#' @export
grid_metrics.LAScatalog = function(las, func, res = 20, start = c(0,0), filter = NULL, by_echo = "all")
{
  # Defensive programming
  if (!is_a_number(res) & !is(res, "RasterLayer")) stop("res is not a number or a RasterLayer")
  if (is_a_number(res)) assert_all_are_non_negative(res)
  assert_is_numeric(start)
  formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!formula) func <- lazyeval::f_capture(func)

  # Compute the alignment option including the case when res is a RasterLayer
  alignment   <- list(res = res, start = start)
  if (is(res, "RasterLayer"))
  {
    ext       <- raster::extent(res)
    r         <- raster::res(res)[1]
    las       <- catalog_intersect(las, res)
    start     <- c(ext@xmin, ext@ymin)
    alignment <- list(res = r, start = start)
  }

  if (opt_chunk_size(las) > 0 && opt_chunk_size(las) < 2*alignment$res)
    stop("The chunk size is too small. Process aborted.", call. = FALSE)

  # Enforce some options
  if (opt_wall_to_wall(las))
    opt_chunk_buffer(las) <- 0.1*alignment[["res"]]

  # Processing
  globals <- future::getGlobalsAndPackages(func)
  options <- list(need_buffer = FALSE, drop_null = TRUE, globals = names(globals$globals), raster_alignment = alignment, automerge = TRUE)
  output  <- catalog_apply(las, grid_metrics, func = func, res = res, start = start, filter = filter, by_echo = by_echo, .options = options)
  return(output)
}

grid_metrics_classic = function(las, layout, call, cells, filter)
{
  .BY <- NULL
  las@data[["cells"]] <- cells

  if (is.null(filter))
  {
    metrics <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = cells]
  }
  else
  {
    filter  <- lasfilter_(las, list(filter))
    metrics <- las@data[filter, if (!anyNA(.BY)) c(eval(call)), by = cells]
  }

  # This may append because new versions of data.table are more flexible than before
  if (any(duplicated(metrics[[1]])))
    stop("Duplicated pixels found. At least one of the metrics was not a number. Each metric should be a single number.", call. = FALSE)

  # Convert the data.table to RasterLayer or RasterBrick
  if (ncol(metrics) == 2L)
  {
    suppressWarnings(layout[metrics[[1]]] <- metrics[[2]])
    names(layout) <- names(metrics)[2]
    return(layout)
  }
  else
  {
    cells <- metrics[[1]]
    metrics[[1]] <- NULL
    nmetrics = ncol(metrics)
    output = raster::brick(layout, nl = nmetrics)
    raster::crs(output) <- crs(las) # patch for raster not updated with rgal 1.5-8
    ncells <- raster::ncell(layout)
    for (i in 1:nmetrics)
    {
      values <- vector(mode = class(metrics[[i]]), length = ncells)
      values[] <- NA
      values[cells] <- metrics[[i]]
      output <- raster::setValues(output, values, layer = i)
    }
    names(output) <- names(metrics)
    return(output)
  }
}

grid_metrics_by_echo_type = function(las, layout, call, cells, filter, by_echo)
{
  .BY <- echo <- . <- NULL

  header <- as.list(las@header)

  if (is.null(las$ReturnNumber) || is.null(las$NumberOfReturns))
    stop("ReturnNumber and NumberOfReturns must be loaded to perform computation by echo type", call. = FALSE)

  if (!rlas::is_valid_NumberOfReturns(las@data, header))
    stop("NumberOfReturns is not valid. See las_check(). Operation aborted.", call. = FALSE)

  if (!rlas::is_compliant_NumberOfReturns(las@data))
    stop("NumberOfReturns is not compliant with ASPRS specification. See las_check(). Operation aborted.", call. = FALSE)

  if (!rlas::is_valid_ReturnNumber(las@data, header))
    stop("ReturnNumber is not valid. See las_check(). Operation aborted.", call. = FALSE)

  if (!rlas::is_compliant_ReturnNumber(las@data))
    stop("ReturnNumber is not compliant with ASPRS specification. See las_check(). Operation aborted.", call. = FALSE)

  if(!rlas::is_compliant_ReturnNumber_vs_NumberOfReturns(las@data))
    stop("ReturnNumber and NumberOfReturns are not compatible. See las_check(). Operation aborted.", call. = FALSE)

  echo_types = character(5)
  echo_types[LASFIRST] <- "first"
  echo_types[LASSINGLE] <- "single"
  echo_types[LASMULTIPLE] <- "multiple"
  echo_types[LASLASTOFMANY] <- "lastofmany"
  echo_types[LASINTERMEDIATE] <- "intermediate"
  echo_types1 <- echo_types[c(LASFIRST, LASINTERMEDIATE, LASLASTOFMANY)]
  echo_types2 <- echo_types[c(LASSINGLE, LASMULTIPLE)]
  echo_class  <- get_echo_type(las$ReturnNumber, las$NumberOfReturns)
  echo_class1 <- echo_class[[1]]
  echo_class2 <- echo_class[[2]]
  compute_type1 <- any(by_echo %in% echo_types1)
  compute_type2 <- any(by_echo %in% echo_types2)

  las@data[["cells"]] <- cells

  if (is.null(filter))
  {
    metrics1 <- NULL
    metrics2 <- NULL

    if (compute_type1)
    {
      las@data[["echo"]] <- echo_class1
      metrics1 <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = c("cells", "echo")]
    }

    if (compute_type2)
    {
      las@data[["echo"]] <- echo_class2
      metrics2 <- las@data[, if (!anyNA(.BY)) c(eval(call)), by = c("cells", "echo")]
    }

    metrics12 <- rbind(metrics1, metrics2)
    data.table::setkey(metrics12, echo)
  }
  else
  {
    filter <- lasfilter_(las, list(filter))

    metrics1 <- NULL
    metrics2 <- NULL

    if (compute_type1)
    {

      metrics1 <- las@data[filter, if (!anyNA(.BY)) c(eval(call)), by = list(cells = cells, echo = echo_class1)]
    }

    if (compute_type2)
      metrics2 <- las@data[filter, if (!anyNA(.BY)) c(eval(call)), by = list(cells = cells, echo = echo_class2)]

    metrics12 <- rbind(metrics1, metrics2)
    data.table::setkey(metrics12, echo)
  }

  # This may append because new versions of data.table are more flexible than before
  if (any(duplicated(metrics12[LASFIRST][[1]])))
    stop("Duplicated pixels found. At least one of the metrics was not a number. Each metric should be a single number.", call. = FALSE)

  raster_list <- vector("list", 5L)
  for (k in 1:5)
  {
    echo_type <- echo_types[k]
    if (!echo_type %in% by_echo) next
    metrics <- metrics12[.(k)]
    metrics <- metrics[, echo := NULL]

    # Convert the data.table to RasterLayer or RasterBrick
    if (ncol(metrics) == 2L)
    {
      suppressWarnings(layout[metrics[[1]]] <- metrics[[2]])
      names(layout) <- paste0(names(metrics)[2], "." , echo_type)
      raster_list[[k]] <- layout
    }
    else
    {
      cells <- metrics[[1]]
      metrics[[1]] <- NULL
      nmetrics = ncol(metrics)
      output = raster::brick(layout, nl = nmetrics)
      raster::crs(output) <- crs(las) # patch for raster not updated with rgal 1.5-8
      ncells <- raster::ncell(layout)
      for (i in 1:nmetrics)
      {
        values <- vector(mode = class(metrics[[i]]), length = ncells)
        values[] <- NA
        values[cells] <- metrics[[i]]
        output <- raster::setValues(output, values, layer = i)
      }
      names(output) <- paste0(names(metrics), "." , echo_type)
      raster_list[[k]] <- output
    }
  }

  raster_list <- Filter(Negate(is.null), raster_list)
  if (length(raster_list) > 1)
    return(raster::brick(raster_list))
  else
    return(raster_list[[1]])
}


get_echo_type <- function(ReturnNumber, NumberOfReturns)
{
  n <- length(ReturnNumber)

  class1 <- rep(LASINTERMEDIATE, n)
  class1[ReturnNumber == 1L] <- LASFIRST
  class1[ReturnNumber == NumberOfReturns & ReturnNumber > 1L] <- LASLASTOFMANY

  class2 <- rep(LASMULTIPLE, n)
  class2[NumberOfReturns == 1L] <- LASSINGLE

  return(list(type1 = class1, type2 = class2))
}


