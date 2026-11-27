#' Point Cloud Decimation Algorithm
#'
#' This function is made to be used in \link{decimate_points}. It implements an algorithm that
#' randomly removes points or pulses to reach the desired density over the area.
#'
#' @param density numeric. The desired output density.
#'
#' @param use_pulse logical. Decimate by removing random pulses instead of random points (requires running
#' \link{retrieve_pulses} first)
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Reach a pulse density of 1 on the overall dataset
#' thinned1 = decimate_points(las, random(1))
#' plot(rasterize_density(las))
#' plot(rasterize_density(thinned1))
#' @return A function
#' @name sample_random
random = function(density, use_pulse = FALSE)
{
  assert_is_a_number(density)
  assert_all_are_positive(density)
  assert_is_a_bool(use_pulse)

  density   <- lazyeval::uq(density)
  use_pulse <- lazyeval::uq(use_pulse)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTDEC, "random")

    if(use_pulse & !"pulseID" %in% names(las))
    {
      warning("No 'pulseID' attribute found. Decimation by points is used.")
      use_pulse <- FALSE
    }

    n <- round(density*area(las))

    if (use_pulse)
      return(.selected_pulses(las@data$pulseID, n))
    else
    {
      if (nrow(las@data) > n)
      {
        idx = sample(1:nrow(las@data), n)
        idx = sort(idx)
        return(idx)
      }
      else
        return(1:nrow(las@data))
    }
  }

  f <- plugin_decimate(f)
  return(f)
}

#' Point Cloud Decimation Algorithm
#'
#' This function is made to be used in \link{decimate_points}. It implements an algorithm that
#' creates a grid with a given resolution and filters the point cloud by randomly selecting some
#' points in each cell. It is designed to produce point clouds that have uniform densities throughout
#' the coverage area. For each cell, the proportion of points or pulses that will be retained is computed
#' using the actual local density and the desired density. If the desired density is greater than the actual
#' density it returns an unchanged set of points (it cannot increase the density). The cell size must be
#' large enough to compute a coherent local density. For example, in a 2 points/m^2 point cloud, 25 square
#' meters would be feasible; however 1 square meter cells would not be feasible because density does
#' not have meaning at this scale.
#'
#' @param density numeric. The desired output density.
#'
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @param use_pulse logical. Decimate by removing random pulses instead of random points (requires running
#' \link{retrieve_pulses} first, unless \code{complete_pulses = TRUE}). See section 'Pulse based thinning'.
#'
#' @param complete_pulses logical. Only used with \code{use_pulse = TRUE}. If \code{TRUE}, a pulse is
#' always kept or removed with all its returns (recommended). Default is \code{FALSE} to keep the
#' results of existing scripts unchanged. See section 'Pulse based thinning'.
#'
#' @section Pulse based thinning:
#' A pulse can have several returns (e.g. one in the canopy and one on the ground). When the laser is
#' tilted, these returns are not at the same place and may fall into different cells of the grid.
#' \itemize{
#' \item With \code{use_pulse = TRUE} only, each cell is thinned on its own. A pulse may then be kept in
#' one cell and removed in the next one, leaving pulses with missing returns.
#' \item With \code{use_pulse = TRUE, complete_pulses = TRUE}, each pulse is counted once, in the cell
#' of its last return, and is then kept or removed with all its returns. The output has the requested
#' number of pulses per cell and no pulses with missing returns. The data must contain \code{gpstime},
#' \code{ReturnNumber} and \code{NumberOfReturns}. Pulses whose last return is not in the data are
#' removed.
#' }
#' With \code{complete_pulses = TRUE} the result is the same each time the code is run (it does not
#' depend on \code{set.seed()}), and processing a \code{LAScatalog} gives the same result as processing
#' the same data loaded in memory. To achieve this with a \code{LAScatalog}, the buffer is increased to
#' \code{res} + \code{opt_chunk_buffer(ctg)} (30 m by default). This is more than enough for usual
#' data. It can be reduced, but should stay larger than the horizontal distance between the first and
#' the last return of a pulse.
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Select points randomly to reach an homogeneous density of 1
#' thinned <- decimate_points(las, homogenize(1,5))
#' plot(rasterize_density(thinned, 10))
#'
#' # Select complete pulses to reach an homogeneous pulse density of 0.5
#' las = readLAS(LASfile, select = "xyzrnt")
#' thinned <- decimate_points(las, homogenize(0.5, 5, use_pulse = TRUE, complete_pulses = TRUE))
#' @return A function
#' @name sample_homogenize
homogenize = function(density, res = 5, use_pulse = FALSE, complete_pulses = FALSE)
{
  assert_is_a_number(density)
  assert_all_are_positive(density)
  assert_is_a_bool(use_pulse)
  assert_is_a_bool(complete_pulses)
  assert_is_a_number(res)
  assert_all_are_positive(res)

  if (complete_pulses && !use_pulse)
    warning("'complete_pulses = TRUE' is ignored because 'use_pulse = FALSE'.", call. = FALSE)

  density   <- lazyeval::uq(density)
  res       <- lazyeval::uq(res)
  use_pulse <- lazyeval::uq(use_pulse)
  complete_pulses <- lazyeval::uq(complete_pulses)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTDEC, "homogenize")

    if (use_pulse && complete_pulses)
    {
      n <- round(density*res^2)
      return(.homogenize_last_return_pulses(las, res, n))
    }

    if (use_pulse & !"pulseID" %in% names(las))
    {
      warning("No 'pulseID' attribute found. Decimation by points is used.")
      use_pulse <- FALSE
    }

    pulseID <- NULL

    n       <- round(density*res^2)
    layout  <- raster_layout(las, res)
    cells   <- get_group(layout, las)

    if (use_pulse)
      return(las@data[, .I[.selected_pulses(pulseID, n)], by = cells]$V1)
    else
      return(las@data[, .I[.selected_pulses(1:.N, n)], by = cells]$V1)
  }

  f <- plugin_decimate(f)
  return(f)
}

#' Point Cloud Decimation Algorithm
#'
#' These functions are made to be used in \link{decimate_points}. They implement algorithms that
#' create a grid with a given resolution and filters the point cloud by selecting the highest/lowest
#' point within each cell.
#'
#' @param res numeric. The resolution of the grid used to filter the point cloud
#'
#' @export
#'
#' @family point cloud decimation algorithms
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyz")
#'
#' # Select the highest point within each cell of an overlayed grid
#' thinned = decimate_points(las, highest(4))
#' #plot(thinned)
#'
#' # Select the lowest point within each cell of an overlayed grid
#' thinned = decimate_points(las, lowest(4))
#' #plot(thinned)
#' @return A function
#' @name sample_maxima
highest = function(res = 1)
{
  assert_is_a_number(res)
  assert_all_are_positive(res)

  res <- lazyeval::uq(res)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTDEC, "highest")
    layout <- raster_layout(las, res)
    return(C_highest(las, layout))
  }

  f <- plugin_decimate(f)
  return(f)
}

#' @family point cloud decimation algorithms
#' @export
#' @name sample_maxima
lowest = function(res = 1)
{
  assert_is_a_number(res)
  assert_all_are_positive(res)

  res <- lazyeval::uq(res)

  f = function(las)
  {
    assert_is_valid_context(LIDRCONTEXTDEC, "lowest")
    layout <- raster_layout(las, res)
    return(C_lowest(las, layout))
  }

  f <- plugin_decimate(f)
  return(f)
}

#' Point Cloud Decimation Algorithm
#'
#' These functions are made to be used in \link{decimate_points}. They implements algorithm that
#' creates a 3D grid with a given resolution and filters the point cloud by selecting
#' points of interest within each voxel. `random_per_voxel()` sample random points. `barycenter_per_voxel()`
#' samples the point that is the closest to the barycenter of the points within a given voxel.
#' `[lowest|highest]_attribute_per_voxel()` sample respectively the point that have the highest/lowest
#' attribute (e.g. Intensity) per voxel.
#'
#' @param res numeric. The resolution of the voxel grid used to filter the point cloud
#' @param n integer. The number of points to select
#' @param attribute string name of an attribute (such as 'intensity')
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz")
#' thinned <- decimate_points(las, random_per_voxel(8, 1))
#' #plot(thinned)
#' @family point cloud decimation algorithms
#' @export
#' @return A function
#' @name sample_per_voxel
random_per_voxel = function(res = 1, n = 1)
{
  assert_all_are_positive(n)
  assert_all_are_positive(res)
  n <- as.integer(n)
  if (length(res) == 1) res <- c(res, res)

  n <- lazyeval::uq(n)
  res <- lazyeval::uq(res)

  f = function(las)
  {
    by <- group_grid_3d(las$X, las$Y, las$Z, res)
    return(las@data[, .I[.selected_pulses(1:.N, n)], by = by]$V1)
  }

  f <- plugin_decimate(f)
  return(f)
}

#' @export
#' @rdname sample_per_voxel
barycenter_per_voxel = function(res = 1)
{
  assert_all_are_positive(res)
  res <- lazyeval::uq(res)

  f = function(las)
  {
    X <- Y <- Z <- NULL

    id = C_voxel_id(las, res)

    which.mean = function(X,Y,Z)
    {
      x = mean(X)
      y = mean(Y)
      z = mean(Z)
      d = (X-x)^2 + (Y-y)^1 + (Z-z)^2
      return(which.min(d))
    }

    return(las@data[, .I[which.mean(X,Y,Z)], by = id]$V1)
  }

  f <- plugin_decimate(f)
  return(f)

}

#' @export
#' @rdname sample_per_voxel
lowest_attribute_per_voxel = function(res, attribute = "Z")
{
  assert_all_are_positive(res)
  assert_is_character(attribute)

  res <- lazyeval::uq(res)
  attribute <- lazyeval::uq(attribute)

  f = function(las)
  {
    if (!attribute %in% names(las))
      stop(paste0(attribute, " is not an attribute in this point cloud"), call. = FALSE)

    tmp <- NULL
    voxelID = C_voxel_id(las, res)
    las@data$tmp = las@data[[attribute]]
    return(las@data[, .I[which.min(tmp)], by = voxelID]$V1)
  }

  f <- plugin_decimate(f)
  return(f)
}

#' @export
#' @rdname sample_per_voxel
highest_attribute_per_voxel = function(res, attribute = "Z")
{
  assert_all_are_positive(res)
  assert_is_character(attribute)
  tmp <- NULL

  res <- lazyeval::uq(res)
  attribute <- lazyeval::uq(attribute)

  f = function(las)
  {
    voxelID = C_voxel_id(las, res)
    las@data$tmp = las@data[[attribute]]
    return(las@data[, .I[which.max(tmp)], by = voxelID]$V1)
  }

  f <- plugin_decimate(f)
  return(f)
}


.selected_pulses = function(pulseID, n)
{
  p <- unique(pulseID)

  if (n > length(p))
    return(rep(TRUE, length(pulseID)))

  selectedPulses <- sample(p, n)
  selectedPulses <- sort(selectedPulses)
  selectedPulses <- pulseID %in% selectedPulses


  return(selectedPulses)
}

# Pulse-based homogenization (#823). Pulses are identified by gpstime and sampled in the cell of a
# global grid (aligned on 0,0, same cells as raster_layout()) that contains their last return. Then all the returns of the selected
# pulses are kept. The sampling uses a seed derived from the cell coordinates so a cell sampled
# in two different chunks of a LAScatalog gives the same result.
.homogenize_last_return_pulses = function(las, res, n)
{
  needed <- c("gpstime", "ReturnNumber", "NumberOfReturns")
  missing <- needed[!needed %in% names(las)]
  if (length(missing) > 0)
    stop(paste0("Attribute(s) ", paste(missing, collapse = ", "), " not found. Pulse based decimation is not possible."), call. = FALSE)

  gpstime  <- las@data[["gpstime"]]
  if (length(gpstime) > 0 && all(gpstime == 0))
    stop("'gpstime' attribute is populated with 0 only. Pulse based decimation is not possible.", call. = FALSE)

  last <- las@data[["ReturnNumber"]] == las@data[["NumberOfReturns"]]
  ref  <- data.table::data.table(
    cx = floor(las@data[["X"]][last] / res),
    cy = ceiling(las@data[["Y"]][last] / res) - 1,  # same convention as raster_layout(): rows from the top
    gpstime = gpstime[last])

  if (nrow(ref) == 0)
    return(integer(0))

  # Use a dedicated RNG and restore the user's RNG state on exit
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  else
  {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }

  sample_cell = function(t, cx, cy)
  {
    t <- sort(unique(t))
    if (length(t) <= n) return(t)
    seed <- (cx * 92821 + cy * 68917) %% 2147483647
    set.seed(as.integer(seed), kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    return(t[sample.int(length(t), n)])
  }

  cx <- cy <- NULL
  selected <- ref[, list(gpstime = sample_cell(gpstime, cx[1], cy[1])), by = list(cx, cy)]$gpstime
  return(which(gpstime %in% selected))
}
