# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
#
# This file is part of lidR R package.
#
# lidR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>
#
# ===============================================================================


#' Apply a grid function over a catalog
#'
#' This function applies over an entire catalog any user-defined function that returns a \code{lasmetrics} object. Used internaly by \link{grid_metrics}, \link{grid_terrain}, \link{grid_canopy} and other
#' \code{grid_*} functions when the input is a \code{LAScatalog}, it ensures continuous rasterization of
#' the dataset and performs pre- and post-processes. This function can be seen as a straightforward
#' 'grid-specific' version of \link{catalog_apply}, which is even more generic.
#'
#' The user-defined function \code{grid_func} must respect a template. Like in \link{grid_metrics},
#' \link{grid_terrain} or \link{grid_canopy}, the user-defined function must have a parameter called
#' \code{x} that will receive a \code{LAS} object and a parameter \code{res} that will receive the
#' resolution of the grid. The parameter \code{start} is optional.
#'
#' @param catalog A \link[lidR:LAScatalog-class]{LAScatalog}
#' @param grid_func A function that returns a \code{lasmetrics} object. This function must follow a
#' specific template (see details and examples)
#' @param res numeric. Resolution of the grid
#' @param select character. The 'select' parameter from \link{readLAS}.
#' @param filter character. The 'filter' parameter from \link{readLAS}.
#' @param start numeric. The 'start' parameter from \link{grid_metrics}
#' @param ... Any other parameter required by \code{grid_func}
#'
#' @return Returns a \code{data.table} containing the metrics for each cell. The table
#' has the class "lasmetrics" enabling easy plotting.
#' @export
#'
#' @examples
#' # This example computes the mean elevation of points above 5 m over an entire
#' # catalog, after removing all points in lakes into a shapefile.
#'
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' shapefile_dir <- system.file("extdata", package = "lidR")
#'
#' ctg = catalog(LASfile)
#' tiling_size(ctg) <- 160
#'
#' lakes = rgdal::readOGR(shapefile_dir, "lake_polygons_UTM17")
#'
#' my_grid_metrics = function(x, res, spdf)
#' {
#'   lasclassify(x, spdf, "inpoly")
#'   x = lasfilter(x, !inpoly)
#'   grid_metrics(x, mean(Z), res)
#' }
#'
#' mean = grid_catalog(ctg, my_grid_metrics, 20,
#'                     select = "xyz", filter = "-drop_z_below 5",
#'                     spdf = lakes)
#'
grid_catalog <- function(catalog, grid_func, res, select = "*", filter = "", start = c(0,0), ...)
{
  stopifnot(is(catalog, "LAScatalog"), is.function(grid_func))

  callparam  <- list(...)
  progress   <- progress(catalog)
  ncores     <- cores(catalog)
  resolution <- res
  funcname   <- lazyeval::expr_text(grid_func)

  # Reduce the catalog with rasters
  # ========================================
  # 'res' may be a RasterLayer. This is a currently undocumented feature. In this case
  # the grid_function is applied only in non-empty cells of the RasterLayer

  if (is(res, "RasterLayer"))
  {
    `Min X` <- `Min Y` <- `Max X` <- `Max Y` <- p <- NULL

    ext = raster::extent(res)
    catalog@data = catalog@data[!(`Min X` >= ext@xmax | `Max X` <= ext@xmin | `Min Y` >= ext@ymax | `Max Y` <= ext@ymin)]

    resolution = raster::res(res)

    if (resolution[1] !=  resolution[2])
      stop("Rasters with different x y resolutions are not supported", call. = FALSE)

    resolution = resolution[1]
  }

  # Test of memory to prevent memory overflow
  # ========================================
  # If the test judges that the output will be too large it can ask the user
  # to make a choice about the processing method

  choice = memory_test(catalog, resolution)

  if (choice == 2)
    vrt(catalog) <- paste0(tempdir(),"/", funcname,"/")
  else if (choice == 3)
    return(invisible())

  # Create a pattern of clusters to be sequentially processed
  # ========================================

  buffer(catalog) <- buffer(catalog) + 0.1
  clusters <- catalog_makecluster(catalog, resolution, start)
  nclust <- length(clusters)
  if (ncores > nclust) ncores = nclust

  # Set up the parameter that will be used for the call
  # =========================================

  # Tweak to enable non-standard evaluation
  if (!is.null(callparam$func))
  {
    if (is.call(callparam$func))
      callparam$func <- as.expression(callparam$func)
  }

  callparam$res <- resolution

  if (any(start != 0))
    callparam$start <- start

  # Create or clean the temporary directory
  if (save_vrt(catalog))
  {
    if (!dir.exists(vrt(catalog)))
      dir.create(vrt(catalog))
    else
      unlink(vrt(catalog), recursive = TRUE) ; dir.create(vrt(catalog))
  }

  # Computation over the entire catalog
  # ========================================
  # Done in parallel. If a single cluster returns an error the process is stopped

  future::plan(future::multiprocess, workers = ncores)

  output = vector("list", nclust)
  for(i in seq_along(clusters))
  {
    cluster = clusters[[i]]

    output[[i]] <- future::future({apply_grid_func(cluster, grid_func, callparam, filter, select) }, substitute = FALSE, earlySignal = TRUE)

    if(progress)
    {
      cat(sprintf("\rProgress: %g%%", round(i/nclust*100)), file = stderr())
      graphics::rect(cluster@bbox$xmin, cluster@bbox$ymin, cluster@bbox$xmax, cluster@bbox$ymax, border = "black", col = "forestgreen")
    }
  }

  if(progress)
    cat("\n")

  output <- future::values(output)

  # Post-process the output
  # ========================================
  # If RasterLayer were written on the disk, build a VRT.
  # Otherwise build the data.table.

  if (!save_vrt(catalog))
  {
    # Return a data.table
    ._class = class(output[[1]])
    output = data.table::rbindlist(output)
    data.table::setattr(output, "class", ._class)
    data.table::setattr(output, "res", resolution)
  }
  else
  {
    # Build virtual raster mosaic and return it
    ras_lst = list.files(vrt(catalog), full.names = TRUE, pattern = ".tif$")
    save_in = paste0(vrt(catalog), "/", funcname, ".vrt")
    gdalUtils::gdalbuildvrt(ras_lst, save_in)
    output = raster::stack(save_in)
  }

  return(output)
}

# Apply a grid_* function for a given ROI of a catlog 
#
# @param X list. the coordinates of the region of interest (rectangular)
# @param grid_func function. the grid_* function to be applied
# @param ctg  LAScatalog.
# @param res numric. the resolution to apply the grid_* function
# @param filter character. the streaming filter to be applied
# @param param list. the parameters of the grid_function except res
apply_grid_func = function(cluster, grid_func, param, filter, select)
{
  X <- Y <- NULL

  # Variables for readability
  xleft   <- cluster@bbox$xmin
  xright  <- cluster@bbox$xmax
  ybottom <- cluster@bbox$ymin
  ytop    <- cluster@bbox$ymax
  name    <- cluster@bbox$name
  path    <- cluster@save
  res     <- param$res

  # Extract the ROI as a LAS object
  las <- readLAS(cluster, filter = filter, select = select)

  # Skip if the ROI falls in a void area
  if (is.null(las))
    return(NULL)

  # Call the function
  param$x   <- las
  metrics   <- do.call(grid_func, args = param)

  if (!is(metrics, "lasmetrics"))
    stop("User-defined function does not return proper data type", call. = FALSE)

  # Remove the buffer
  metrics <- metrics[X >= xleft+0.5*res & X <= xright-0.5*res & Y >= ybottom+0.5*res & Y <= ytop-0.5*res]
  as.lasmetrics(metrics, res)

  # Return results or write file
  if (cluster@save == "")
  {
    return(metrics)
  }
  else
  {
    if (nrow(metrics) == 0)
      return(NULL)

    metrics <- as.raster(metrics)
    raster::writeRaster(metrics, path, format = "GTiff", NAflag = -Inf)
    return(NULL)
  }
}

memory_test = function(catalog, resolution)
{
  surface <- area(catalog)
  npixel  <- surface / (resolution*resolution)
  nmetric <- 3 # Must find a way to access this number
  nbytes  <- npixel * nmetric * 8
  class(nbytes) <- "object_size"

  if (nbytes > LIDROPTIONS("memlimit") & save_vrt(catalog))
  {
    size = format(nbytes, "auto")
    text = paste0("The process is expected to return an approximately ", size, " object. It might be too much.\n")
    choices = c(
      "Proceed anyway",
      "Store the results on my disk and return a virtual raster mosaic",
      "Abort, let me configure myself with catalog options (see ?catalog)'")

    cat(text)
    choice = utils::menu(choices)
    return(choice)
  }

  return(0)
}