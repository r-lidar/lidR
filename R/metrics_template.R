#' Metric derivation at different levels of regularization
#'
#' `template_metrics()` computes a series of user-defined descriptive statistics for a LiDAR dataset
#' within each element of a template. Depending on the template it can be for each pixel of a raster
#' (area-based approach), or each polygon, or each segmented tree, or on the whole point cloud. Other
#' functions are convenient and simplified wrappers around `template_metrics()` and are expected to be
#' the actual functions used. See Details and Examples.
#'
#' \describe{
#' \item{`pixel_metrics`}{Area-based approach. Computes metrics in a square tessellation. The output is a
#' raster.}
#' \item{`hexagon_metrics`}{Computes metrics in an hexagon tessellation. The output is a `sf/sfc_POLYGON`}
#' \item{`plot_metrics`}{Computes metrics for each plot of a ground inventory by 1. clipping the plot
#' inventories with \link{clip_roi}, 2. computing the user's metrics for each plot with  \link{cloud_metrics}, and
#' 3. combining spatial data and metrics into one data.frame ready for statistical modelling with
#' `cbind`. The output is of the class of the input.}
#' \item{`cloud_metrics`}{Computes a series of user-defined descriptive statistics for an entire point cloud.
#' The output is a `list`}
#' \item{`crown_metrics`}{Once the trees are segmented, i.e. attributes exist in the
#' point cloud that reference each tree, computes a set of user-defined descriptive statistics for
#' each individual tree. The output can be spatial points or spatial polygons (`sf/sfc_POINT` or `sf/sfc_POLYGON`)}
#' \item{`voxel_metrics`}{Is a 3D version of `pixel_metrics`. It creates a 3D matrix of voxels with a given
#' resolution. It creates a voxel from the cloud of points if there is at least one point. The output is
#' a `data.frame`}
#' \item{`point_metrics`}{Is a bit more complex and is documented in \link{point_metrics}}
#' }
#'
#' @template param-las
#' @param func formula or expression. An expression to be applied to each element of the template (see
#' section "Parameter func").
#' @param template can be of many types and corresponds to the different levels of regularization.
#' `RasterLayer/stars/SpatRaster`, `sf/sfc` (polygons), `numeric`, `bbox`, `NULL`. The metrics are
#' computed for each element of the template. See examples.
#' @param filter formula of logical predicates. Enables the function to run only on points of interest
#' in an optimized way. See examples.
#' @param by_echo characters. The metrics are computed multiple times for different echo types. Can
#' be one or more of "all", "first", "intermediate", "lastofmany", "single", and "multiple". See examples.
#' Default is "all" meaning that it computes metrics with all points provided.
#' @param ... propagated to `template_metrics` i.e. `filter` and `by_echo`. `pixel_metrics()` also
#' supports `pkg = "terra|raster|stars"` to get an output in `SpatRaster`, `Raster*`
#' or `stars` format. Default is `getOption("lidR.raster.default")`.
#'
#' @section Parameter \code{func}:
#' The function to be applied to each cell is a classical function (see examples) that
#' returns a labelled list of metrics. For example, the following function \code{f} is correctly formed.
#' \preformatted{
#' f = function(x) {list(mean = mean(x), max = max(x))}
#' }
#' And could be applied either on the \code{Z} coordinates or on the intensities. These two
#' statements are valid:
#' \preformatted{
#' pixel_metrics(las, f(Z), res = 20)
#' voxel_metrics(las, f(Intensity), res = 2)
#' }
#' The following existing functions allow the user to
#' compute some predefined metrics: \link[=stdmetrics]{stdmetrics}
#' \link[=entropy]{entropy}, \link[=VCI]{VCI}, \link[=LAD]{LAD}. But usually users must write their own
#' functions to create metrics. \code{template_metrics} will dispatch the point cloud in the user's
#' function.
#'
#' @return Depends on the function, the template and the number of metrics. Can be a `RasterLayer`,
#' a `RasterBrick`, a `stars`, a `SpatRaster` a `sf/sfc`, a `list`, a `SpatialPolygonDataFrame`, or
#' a `data.table`. Functions are supposed to return an object that is best suited for storing the level
#' of regularization needed.
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-keep_random_fraction 0.5")
#' col <- sf::sf.colors(15)
#' fun1 <- ~list(maxz = max(Z))
#' fun2 <- ~list(q85 = quantile(Z, probs = 0.85))
#'
#' # ================
#' # TEMPLATE METRICS
#' # ================
#'
#' # a raster as template
#' template <- raster::raster(extent(las), nrow = 15, ncol = 15)
#' raster::crs(template) <- crs(las)
#' m <- template_metrics(las, fun1, template)
#' plot(m, col = col)
#'
#' # a sfc_POLYGON as template
#' sfc <- sf::st_as_sfc(st_bbox(las))
#' template <- sf::st_make_grid(sfc, cellsize = 20, square = FALSE)
#' m <- template_metrics(las, fun1, template)
#' plot(m)
#'
#' # a bbox as template
#' template <- st_bbox(las) + c(50,30,-50,-70)
#' plot(sf::st_as_sfc(st_bbox(las)), col = "gray")
#' plot(sf::st_as_sfc(template), col = "darkgreen", add = TRUE)
#' m <- template_metrics(las, fun2, template)
#' print(m)
#'
#' # ================
#' # CUSTOM METRICS
#' # ================
#'
#' # Define a function that computes custom metrics
#' # in an R&D perspective.
#' myMetrics = function(z, i) {
#'   metrics = list(
#'      zwimean = sum(z*i)/sum(i), # Mean elevation weighted by intensities
#'      zimean  = mean(z*i),       # Mean products of z by intensity
#'      zsqmean = sqrt(mean(z^2))) # Quadratic mean
#'
#'    return(metrics)
#' }
#'
#' # example with a stars template
#' template <- stars::st_as_stars(st_bbox(las), dx = 10, dy = 10)
#' m <- template_metrics(las, myMetrics(Z, Intensity), template)
#' plot(m, col = col)
#'
#' # ================
#' # CLOUD METRICS
#' # ================
#'
#' cloud_metrics(las, .stdmetrics_z)
#'
#' # ================
#' # PIXEL METRICS
#' # ================
#'
#' m <- pixel_metrics(las, fun1, 20)
#' plot(m, col = col)
#'
#' m = pixel_metrics(las, myMetrics(Z, Intensity))
#' plot(m, col = col)
#'
#' # ================
#' # PLOT METRICS
#' # ================
#'
#' shpfile <- system.file("extdata", "efi_plot.shp", package="lidR")
#' inventory <- sf::st_read(shpfile, quiet = TRUE)
#' inventory # contains an ID and a Value Of Interest (VOI) per plot
#'
#' m <- plot_metrics(las, fun2, inventory, radius = 11.28)
#' plot(header(las))
#' plot(m["q85"], pch = 19, cex = 3, add = TRUE)
#'
#' \donttest{
#' # Works with polygons as well
#' inventory <- sf::st_buffer(inventory, 11.28)
#' plot(header(las))
#' plot(sf::st_geometry(inventory), add = TRUE)
#' m <- plot_metrics(las, .stdmetrics_z, inventory)
#' plot(m["zq85"], pch = 19, cex = 3, add = TRUE)
#' }
#'
#' # ================
#' # VOXEL METRICS
#' # ================
#'
#' m <- voxel_metrics(las, length(Z), 8)
#' m <- voxel_metrics(las, mean(Intensity), 8)
#' #plot(m, color = "V1", colorPalette = heat.colors(50), trim = 60)
#' #plot(m, color = "V1", colorPalette = heat.colors(50), trim = 60, voxel = TRUE)
#'
#' # ================
#' # CROWN METRICS
#' # ================
#'
#' # Already tree-segmented point cloud
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' trees <- readLAS(LASfile, filter = "-drop_z_below 0")
#'
#' metrics <- crown_metrics(trees, .stdtreemetrics)
#' plot(metrics["Z"], pch = 19)
#'
#' metrics <- crown_metrics(trees, .stdtreemetrics, geom = "convex")
#' plot(metrics["Z"])
#'
#' metrics <- crown_metrics(trees, .stdtreemetrics, geom = "bbox")
#' plot(metrics["Z"])
#'
#' \donttest{
#' metrics <- crown_metrics(trees, .stdtreemetrics, geom = "concave")
#' plot(metrics["Z"])
#' }
#'
#' # ================
#' # ARGUMENT FILTER
#' # ================
#'
#' # Compute using only some points: basic
#' first = filter_poi(las, ReturnNumber == 1)
#' metrics = pixel_metrics(first, mean(Z), 20)
#'
#' # Compute using only some points: optimized
#' # faster and uses less memory. No intermediate object
#' metrics = pixel_metrics(las, mean(Z), 20, filter = ~ReturnNumber == 1)
#'
#' # Compute using only some points: best
#' # ~50% faster and uses ~10x less memory
#' las = readLAS(LASfile, filter = "-keep_first")
#' metrics = pixel_metrics(las, mean(Z), 20)
#'
#' # ================
#' # ARGUMENT BY_ECHO
#' # ================
#'
#' func = ~list(avgI = mean(Intensity))
#' echo = c("all", "first","multiple")
#'
#' # func defines one metric but 3 are computed respectively for: (1) all echo types,
#' # (2) for first returns only and (3) for multiple returns only
#' metrics <- pixel_metrics(las, func, 20, by_echo = echo)
#' plot(metrics, col = heat.colors(25))
#'
#' cloud_metrics(las, func, by_echo = echo)
#'
#' @name aggregate
#' @rdname aggregate
#' @md
NULL

#' @export
#' @rdname aggregate
template_metrics <- function(las, func, template, filter = NULL, by_echo = "all", ...)
{
  UseMethod("template_metrics", las)
}

#' @export
template_metrics.LAS <- function(las, func, template, filter = NULL, by_echo = "all", ...)
{
  . <- echo <- NULL

  # Defensive programming
  if (!inherits(las, "LAS")) stop("template metrics only supports LAS objects", call. = FALSE)
  formula <- tryCatch(lazyeval::is_formula(func), error = function(e) FALSE)
  if (!formula) func <- lazyeval::f_capture(func)
  echo_types <- c("all", "first", "intermediate", "lastofmany", "single", "multiple")
  stopifnot(all(by_echo %in% echo_types))

  dots <- list(...)
  pkg <- if (is.null(dots$pkg)) getOption("lidR.raster.default") else dots$pkg

  # Compute on different subsets of echos
  all_echos   <- any(by_echo == "all")
  split_echos <- any(by_echo != "all")

  # User-defined call parsing
  func   <- lazyeval::f_interp(func)
  call   <- lazyeval::as_call(func)

  # The way the user's expression is evaluated makes the scoping rule counterintuitive and
  # somewhat buggy. It gives precedence to lidR in all cases. For example, it will call an internal
  # function of lidR instead of its globalenv version if they have the same name. The following
  # tries to parse the expression and evaluates whether ambiguous definitions exist.
  call_names <- all.names(call)
  las_names  <- names(las)
  call_names <- call_names[!call_names %in% las_names]
  lapply(call_names, stop_if_ambiguous_definition)

  call <- deparse(call)
  call <- trimws(call)
  call <- paste0(call, collapse = " ")

  if (is.null(template)) template <- st_bbox(las)

  # Pre-computation of the groups
  grp  <- get_group(template, las)

  # If the template is not numeric it is a spatial object with a bbox
  # We check if the template matches the point cloud
  if (!is.numeric(template))
  {
    bbox_template <- sf::st_as_sfc(sf::st_bbox(template))
    bbox_template <- sf::st_set_crs(bbox_template, sf::NA_crs_)     # Workaround for RasterLayer
    bbox_template <- sf::st_set_crs(bbox_template, sf::st_crs(las)) # Workaround for RasterLayer
    bbox_las      <- sf::st_as_sfc(st_bbox(las))
    i <- sf::st_intersects(bbox_template, bbox_las)

    if (is(template, "raster_template"))
      template <- raster_materialize(template, pkg = pkg)

    if (length(i[[1]]) == 0L)
    {
      if (is_raster(template))
      {
        # For backward compatibility we return the template with NAs
        # It means that pixel_metrics and grid_metrics never fails
        warning("No points fall in the raster. Bounding boxes are not intersecting.", call. = FALSE)
        return(template)
      }

      stop("Bounding boxes of the template and the point-cloud are not intersecting.", call. = FALSE)
    }
  }

  # Aggregation of the point cloud
  if (all_echos)
    M1 <- metrics_classic(las, call, grp, filter)

  if (split_echos)
    M2 <- metrics_by_echo_type(las, call, grp, filter, by_echo)

  # Merge subsets
  if (all_echos && split_echos)
    M <- rbind(M1, M2)
  else if (all_echos)
    M <- M1
  else if (split_echos)
    M <- M2
  else
    stop("Internal error") #nocov

  data.table::setDT(M)
  data.table::setkey(M, echo)

  # Cast the table into appropriate objects
  computed_echos <- sort(match(by_echo, echo_types) - 1L)
  output <- vector("list", length(computed_echos))
  i <- 1
  for (k in computed_echos)
  {
    echo_type <- echo_types[k+1]
    m <- M[.(k)]
    cells <- m[[1]]
    m[, cells := NULL]
    m[, echo := NULL]
    if (k > 0) names(m) <- paste0(names(m), "." , echo_type)
    output[[i]] <- assign_to_template(template, cells, m, pkg = pkg)
    i <- i + 1
  }
  output <- merge_list(template, output)

  return(output)
}

# ==== CORE ====

metrics_classic = function(las, call, cells, filter)
{
  .BY <- NULL
  las@data[["cells"]] <- cells
  las@data[["echo"]] <- LASALLECHOS

  verbose("Computing metrics for all echos...")

  if (is.null(filter))
  {
    verbose("Argument filter is NULL...")
    cmd <- paste0('las@data[, if (!anyNA(.BY)) ', call, ', by = .(cells, echo)]')
  }
  else
  {
    filter  <- lasfilter_(las, list(filter))
    verbose(glue::glue("Argument filter retained {sum(filter)} points..."))
    cmd <- paste0('las@data[filter, if (!anyNA(.BY)) ', call, ', by = .(cells, echo)]')
  }

  metrics <- eval(parse(text = cmd))

  # This may append because new versions of data.table are more flexible than before
  if (any(duplicated(metrics[[1]])))
    stop("Duplicated elements found. At least one of the metrics was not a number. Each metric should be a single number.", call. = FALSE)

  return(metrics)
}

metrics_by_echo_type = function(las, call, cells, filter, by_echo)
{
  .BY <- echo <- . <- NULL

  header <- as.list(header(las))

  # Somewhat aggressive defensive programming
  #nocov start
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
  #nocov end

  echo_types = character(5)
  echo_types[LASFIRST] <- "first"
  echo_types[LASSINGLE] <- "single"
  echo_types[LASMULTIPLE] <- "multiple"
  echo_types[LASLASTOFMANY] <- "lastofmany"
  echo_types[LASINTERMEDIATE] <- "intermediate"
  echo_types1 <- echo_types[c(LASFIRST, LASINTERMEDIATE, LASLAST)]
  echo_types2 <- echo_types[c(LASSINGLE, LASMULTIPLE)]
  echo_class  <- get_echo_type(las$ReturnNumber, las$NumberOfReturns)
  echo_class1 <- echo_class[[1]]
  echo_class2 <- echo_class[[2]]
  compute_type1 <- any(by_echo %in% echo_types1)
  compute_type2 <- any(by_echo %in% echo_types2)

  las@data[["cells"]] <- cells

  verbose("Computing metrics for selected echos...")

  if (is.null(filter))
  {
    verbose("Argument filter is NULL...")

    metrics1 <- NULL
    metrics2 <- NULL

    if (compute_type1)
    {
      verbose("Computing metrics for first intermediate and last...")
      las@data[["echo"]] <- echo_class1
      cmd <- paste0('las@data[, if (!anyNA(.BY)) ', call, ', by = .(cells, echo)]')
      metrics1 <- eval(parse(text = cmd))
    }

    if (compute_type2)
    {
      verbose("Computing metrics for single and multiple...")
      las@data[["echo"]] <- echo_class2
      cmd <- paste0('las@data[, if (!anyNA(.BY)) ', call, ', by = .(cells, echo)]')
      metrics2 <-eval(parse(text = cmd))
    }

    metrics12 <- rbind(metrics1, metrics2)
    data.table::setkey(metrics12, echo)
  }
  else
  {
    filter <- lasfilter_(las, list(filter))
    cells <- cells[filter]
    echo_class1 <- echo_class1[filter]
    echo_class2 <- echo_class2[filter]

    verbose(glue::glue("Argument filter retained {sum(filter)} points..."))

    metrics1 <- NULL
    metrics2 <- NULL

    if (compute_type1)
    {
      cmd <- paste0('las@data[filter, if (!anyNA(.BY)) ', call, ', by = list(cells = cells, echo = echo_class1)]')
      metrics1 <- eval(parse(text = cmd))
    }

    if (compute_type2)
    {
      cmd <- paste0('las@data[filter, if (!anyNA(.BY)) ', call, ', by = list(cells = cells, echo = echo_class2)]')
      metrics1 <- eval(parse(text = cmd))
    }

    metrics12 <- rbind(metrics1, metrics2)
    data.table::setkey(metrics12, echo)
  }

  # This may append because new versions of data.table are more flexible than before
  if (any(duplicated(metrics12[LASFIRST][[1]])))
    stop("Duplicated elements found. At least one of the metrics was not a number. Each metric should be a single number.", call. = FALSE)

  return(metrics12)
}

get_echo_type <- function(ReturnNumber, NumberOfReturns)
{
  n <- length(ReturnNumber)

  class1 <- rep(LASINTERMEDIATE, n)
  class1[ReturnNumber == 1L] <- LASFIRST
  class1[ReturnNumber == NumberOfReturns & ReturnNumber > 1L] <- LASLAST

  class2 <- rep(LASMULTIPLE, n)
  class2[NumberOfReturns == 1L] <- LASSINGLE

  return(list(type1 = class1, type2 = class2))
}

# ==== GROUPING ====

get_group <- function(template, las) { UseMethod("get_group", template) }

get_group.stars <- function(template, las)
{
  return(raster_cell_from_xy(template, las$X, las$Y))
}

get_group.RasterLayer <- function(template, las)
{
  return(raster_cell_from_xy(template, las$X, las$Y))
}

get_group.SpatRaster <- function(template, las)
{
  return(raster_cell_from_xy(template, las$X, las$Y))
}

get_group.raster_template <- function(template, las)
{
  return(raster_cell_from_xy(template,las$X, las$Y))
}

get_group.sfc <- function(template, las)
{
  return(point_in_polygons(las, template))
}

get_group.bbox <- function(template, las)
{
  group <- integer(length(las$x))
  group[] <- NA_integer_
  group[las$X >= template$xmin & las$X <= template$xmax &
        las$Y >= template$ymin & las$Y <= template$ymax] <- 1L
  return(group)
}

get_group.numeric <- function(template, las)
{
  if (length(template) != length(las$X))
    stop("Length of template is not the size of the point cloud", call. = FALSE)

  return(template)
}

# ==== ASSIGN ====

assign_to_template <- function(template, cells, metrics, ...) { UseMethod("assign_to_template", template) }

assign_to_template.RasterLayer <- function(template, cells, metrics, ...)
{
  suppressWarnings(template[] <- NA_real_)
  nmetrics <- ncol(metrics)

  if (nmetrics == 1L)
  {
    template[cells] <- metrics[[1]]
    raster_names(template) <- names(metrics)
    return(template)
  }

  suppressWarnings(output <- raster::brick(template, nl = nmetrics))
  ncells <- raster_ncell(template)
  for (i in 1:nmetrics)
  {
    values <- vector(mode = class(metrics[[i]]), length = ncells)
    values[] <- NA_real_
    values[cells] <- metrics[[i]]
    output <- raster::setValues(output, values, layer = i)
  }

  raster_names(output) <- names(metrics)
  return(output)
}

assign_to_template.stars <- function(template, cells, metrics, ...)
{
  n <- ncol(metrics)
  s <- dim(template)

  if (n == 1L)
  {
    template[[1]][cells] <- metrics[[1]]
    raster_names(template) <- names(metrics)
    return(template)
  }

  bbox   <- sf::st_bbox(template)
  output <- stars::st_as_stars(bbox, nx = s[1], ny = s[2], value = NA_real_, nz = n)
  raster_names(output) <- names(metrics)
  for (i in 1:n) output[[1]][,,i][cells] <- metrics[[i]]
  return(output)
}

assign_to_template.SpatRaster <- function(template, cells, metrics, ...)
{
  nmetrics <- ncol(metrics)
  output <- terra::init(template, NA_real_)
  output[cells] <- metrics[[1]]

  if (nmetrics == 1L)
  {
    raster_names(output) <- names(metrics)
    return(output)
  }

  rlist <- vector("list", nmetrics)
  rlist[[1]] <- output

  for (i in 2:nmetrics)
  {
    terra::set.values(template, cells, metrics[[i]])
    rlist[[i]] <- terra::deepcopy(template)
  }

  output <- terra::rast(rlist)

  # Best option but add is not part of terra if library(terra) is not called
  #for (i in 2:nmetrics)
  #{
  #  terra::set.values(template, cells, metrics[[i]])
  #  add(output) <- template
  #}

  raster_names(output) <- names(metrics)
  return(output)
}

assign_to_template.raster_template <- function(template, cells, metrics, ...)
{
   dots <- list(...)
   pkg <- if (is.null(dots$pkg)) getOption("lidR.raster.default") else dots$pkg
   template <- raster_materialize(template, pkg = pkg)
   return(assign_to_template(template, cells, metrics, ...))
}

assign_to_template.sfc <- function(template, cells, metrics, ...)
{
  data.table::setDF(metrics)

  template <- sf::st_as_sf(template)
  for(j in names(metrics))
  {
    template[[j]] <- NA
    template[[j]][cells] <- metrics[[j]]
  }

  geom <- sf::st_geometry(template)
  template <- sf::st_drop_geometry(template)
  template <- sf::st_set_geometry(template, geom)
  return(template)
}

assign_to_template.bbox <- function(template, cells, metrics, ...)
{
  return(as.list(metrics))
}

assign_to_template.numeric <- function(template, cells, metrics, ...)
{
  GRPID <- NULL
  metrics[, GRPID := cells]
  data.table::setcolorder(metrics, c("GRPID"))
  data.table::setkey(metrics, GRPID)
  return(metrics)
}

# ==== MERGE ====

merge_list <- function(template, list)  { UseMethod("merge_list", template) }

merge_list.raster_template <- function(template, list)
{
  template <- raster_materialize(template)
  return(merge_list(template, list))
}

merge_list.RasterLayer <- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  if (length(list) > 1)
    return(raster::brick(list))
  else
    return(list[[1]])
}

merge_list.SpatRaster <- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  if (length(list) > 1)
    return(terra::rast(list))
  else
    return(list[[1]])
}

merge_list.stars <- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  n <- length(list)

  if (n == 1) return(list[[1]])

  dims  <- sapply(list, dim)
  multilayer <- nrow(dims) == 3

  if (multilayer)
  {
    names <- unlist(lapply(list, function(x) stars::st_dimensions(x)$z$value))
    nlayers <- sum(dims[3,])
    output <- list[[1]]
    for (i in 2:n) output <- c(output, list[[i]], along = 3L)
  }
  else
  {
    names <- sapply(list, names)
    nlayers <- n
    bs <- raster_template(template)
    output <- stars::st_as_stars(sf::st_bbox(template), dx = bs$xres, dy = bs$yres, nz = nlayers, values = NA_real_)
    for (i in 1:n) output[[1]][,,i] <- list[[i]][[1]]
  }

  raster_names(output) <- names

  return(output)
}

merge_list.sfc <- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  n <- length(list)

  if (n == 1) return(list[[1]])

  geom <- sf::st_geometry(list[[1]])
  output <- sf::st_drop_geometry(list[[1]])
  for (i in 2:length(list)) output <- cbind(output, sf::st_drop_geometry(list[[i]]))
  output <- sf::st_set_geometry(output, geom)
  return(output)
}


merge_list.bbox<- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  if (length(list) > 1)
    return(do.call(c, list))
  else
    return(list[[1]])
}

merge_list.numeric <- function(template, list)
{
  list <- Filter(Negate(is.null), list)
  n <- length(list)

  if (n == 1) return(list[[1]])

  output <- list[[1]]
  for (i in 2:length(list)) output <- merge(output, list[[i]], all = TRUE)
  return(output[])

}


# ==== ERROR ====

stop_if_ambiguous_definition <- function(name)
{
  # Check if this name exists in lidR (internal or not)
  u <- tryCatch(utils::getFromNamespace(name, 'lidR'), error = function(e) FALSE)
  u <- is.function(u)

  # If FALSE, no ambiguity
  if (!u) return(invisible(NULL))

  # Check in the parent frame if the name has been defined
  e <- parent.frame(1)
  u <- tryCatch(mget(name, envir = e), error = function(e) FALSE)
  u <- !isFALSE(u)
  n <- environmentName(e)
  if (n == "") n <- "parent.frame(1)"

  # If TRUE, there is an ambiguity, two objects have the same name, lidR object will take
  # the precedence. We don't want that
  if (u) stop(glue::glue("The function '{name}' exists in the package lidR but is also defined in {n}. The scoping rules give precedence to lidR and the result may not be the one expected. Please rename your function."), call. = FALSE)

  # We do that while we did not reach R_GlobalEnv
  i <- 2
  while(!identical(parent.frame(i-1), globalenv()) & isFALSE(u))
  {
    e <- parent.frame(i)
    u <- tryCatch(mget(name, envir = e), error = function(e) FALSE)
    u <- !isFALSE(u)
    n <- environmentName(e)
    i <- i+1
  }

  # u is FALSE, we did not find ambiguous definitions
  if (!u) return(invisible(NULL))

  if (n == "") n <- glue::glue("parent.frame({i-1})")

  # u is TRUE, we found ambiguous definitions
  stop(glue::glue("The function '{name}' exists in the package lidR but is also defined in {n}. The scoping rules give precedence to lidR and the result may not be the one expected. Please rename your function."), call. = FALSE)
}

