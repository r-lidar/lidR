#' Individual tree detection
#'
#' Individual tree detection function that find the position of the trees using several possible
#' algorithms.
#'
#' @param las An object of class `LAS` or `LAScatalog`. Can also be a  raster from `raster`, `stars` or `terra`
#' representing a canopy height model, in which case it is processed like a regularly-spaced point cloud.
#' @param algorithm An algorithm for individual tree detection. lidR has: \link{lmf} and \link{manual}.
#' More experimental algorithms may be found in the package \href{https://github.com/Jean-Romain/lidRplugins}{lidRplugins}.
#' @param uniqueness character. A method to compute a unique ID. Can be 'incremental', 'gpstime' or
#' 'bitmerge'. See section 'Uniqueness'. This feature must be considered as 'experimental'.
#'
#' @template section-uniqueness
#'
#' @return `locate_trees` returns an sf object with POINT Z geometries. The table of attributes
#' contains a column `treeID` with an individual ID for each tree. The height of the trees (`Z`) are
#' also repeated in the table of attribute to be analysed as an attribute and not as a coordinate.
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz", filter = "-inside 481250 3812980 481300 3813030")
#'
#' ttops <- locate_trees(las, lmf(ws = 5))
#'
#' #plot(las) |> add_treetops3d(ttops)
#' @md
locate_trees = function(las, algorithm, uniqueness = 'incremental')
{
  UseMethod("locate_trees", las)
}

#' @export
locate_trees.LAS = function(las, algorithm, uniqueness = 'incremental')
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_itd(algorithm)
  match.arg(uniqueness, c('incremental', 'gpstime', 'bitmerge'))

  if (uniqueness == 'gpstime' && !"gpstime" %in% names(las))
    stop("Impossible to compute unique IDs using gpstime: no gpstime found.", call. = FALSE)

  if (uniqueness == 'gpstime' &&  fast_countequal(las@data[["gpstime"]], 0L) == npoints(las))
    stop("Impossible to compute unique IDs using gpstime: gpstime is not populated.", call. = FALSE)

  lidR.context <- "locate_trees"
  res <- algorithm(las)

  if (is(res, "SpatialPointsDataFrame"))
    res <- sf::st_as_sf(res)

  if (is(res, "sf"))
  {
    sf::st_agr(res) <- "constant"
    return(res)
  }

  if (!is.logical(res) & !is.integer(res))
    stop("The output of the algorithm is incorrect")

  maxima <- las@data[res, c("X", "Y", "Z")]
  no_maxima <- nrow(maxima) == 0

  if (no_maxima)
  {
    data   <- data.frame(x = 0, y = 0, z = 0, treeID = 0L, Z = 0)
    output <- sf::st_as_sf(data, coords = c("x", "y", "z"), crs = st_crs(las))
    output <- output[0,]
  }
  else
  {
    if (uniqueness == "incremental")
    {
      ids <- 1:nrow(maxima)
    }
    else if (uniqueness == "gpstime")
    {
      ids <- las@data[["gpstime"]][res]
    }
    else
    {
      xoffset <- las[["X offset"]]
      yoffset <- las[["Y offset"]]
      zoffset <- las[["Z offset"]]

      xscale  <- las[["X scale factor"]]
      yscale  <- las[["Y scale factor"]]
      zscale  <- las[["Z scale factor"]]

      xscaled <- as.integer((maxima[["X"]] - xoffset)/xscale)
      yscaled <- as.integer((maxima[["Y"]] - yoffset)/yscale)

      ids <- bitmerge(xscaled, yscaled)
    }

    data   <- data.frame(x = maxima[["X"]], y = maxima[["Y"]], z = maxima[["Z"]], treeID = ids, Z = maxima[["Z"]])
    output <- sf::st_as_sf(data, coords = c("x", "y", "z"), crs = st_crs(las))
  }

  sf::st_agr(output) <- "constant"
  return(output)
}

#' @export
locate_trees.RasterLayer = function(las, algorithm, uniqueness = 'incremental')
{
  if (raster_is_proxy(las))
  {
    if (!raster_fits_in_memory(las, n = 2))
      stop("Large on-disk rasters are not supported by locate_tree. Load the raster manually.", call. = FALSE)
    else
      las <- raster_in_memory(las)
  }

  las <- raster_as_las(las)
  return(locate_trees(las, algorithm))
}

#' @export
locate_trees.stars = function(las, algorithm, uniqueness = 'incremental')
{
  if (raster_is_proxy(las))
  {
    if (!raster_fits_in_memory(las, n = 2))
      stop("Large on-disk rasters are not supported by locate_tree. Load the raster manually.", call. = FALSE)
    else
      las <- raster_in_memory(las)
  }

  las <- raster_as_las(las)
  return(locate_trees(las, algorithm))
}

#' @export
locate_trees.SpatRaster = function(las, algorithm, uniqueness = 'incremental')
{
  if (raster_is_proxy(las))
  {
    if (!raster_fits_in_memory(las, n = 2))
      stop("Large on-disk rasters are not supported by locate_tree. Load the raster manually.", call. = FALSE)
    else
      las <- raster_in_memory(las)
  }

  las <- raster_as_las(las)
  return(locate_trees(las, algorithm))
}

#' @export
locate_trees.LAScatalog = function(las, algorithm, uniqueness = 'incremental')
{
  if (uniqueness == "gpstime")
    opt_select(las) <- "xyzt"
  else
    opt_select(las) <- "xyz"

  options <- list(need_buffer = TRUE)
  output  <- catalog_map(las, locate_trees, algorithm = algorithm, uniqueness = uniqueness, .options = options)
  return(output)
}

