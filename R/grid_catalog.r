# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016 Jean-Romain Roussel
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

grid_catalog <- function(catalog, grid_func, res, select, filter, ...)
{
  Min.X <- Min.Y <- Max.X <- Max.Y <- p <- pbar <- NULL

  # ========================================
  # Store some stuff in readable variables
  # ========================================

  callparam <- list(...)
  funcname  <- lazyeval::expr_text(grid_func)
  exportdir <- tempdir() %+%  "/" %+% funcname %+% "/"

  LIDROPTIONS(progress = FALSE) # Disable functions progress bars
  progress  <- CATALOGOPTIONS("progress")
  numcores  <- CATALOGOPTIONS("multicore")
  savevrt   <- CATALOGOPTIONS("return_virtual_raster")
  memlimwar <- CATALOGOPTIONS("memory_limit_warning")
  buffer    <- CATALOGOPTIONS("buffer")
  by_file   <- CATALOGOPTIONS("by_file")

  # ========================================
  # Test of memory to prevent memory overflow
  # ========================================

  surface <- sum(with(catalog, (Max.X - Min.X) * (Max.Y - Min.Y)))
  npixel  <- surface / (res*res)
  nmetric <- 3 # Must find a way to access this number
  nbytes  <- npixel * nmetric * 8
  class(nbytes) <- "object_size"

  if (nbytes > memlimwar & !savevrt)
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
      savevrt = TRUE
    else if (choice == 3)
      return(invisible())
  }

  # ========================================
  # Create a pattern of clusters to be
  # sequentially processed
  # ========================================

  ctg_clusters <- catalog_makecluster(catalog, res, buffer, by_file)
  ctg_clusters <- apply(ctg_clusters, 1, as.list)

  # Add the path to the saved file (if saved)
  ctg_clusters <- lapply(ctg_clusters, function(x)
  {
    x$path <- exportdir %+% funcname %+% "_ROI" %+% x$name %+% ".tiff"
    return(x)
  })

  if (numcores > length(ctg_clusters))
    numcores = length(ctg_clusters)

  # =========================================
  # Some settings
  # =========================================

  # Tweak to enable non-standard evaluation
  if (!is.null(callparam$func))
  {
    if (is.call(callparam$func))
      callparam$func <- as.expression(callparam$func)
  }

  # Enable progress bar working even with multicore
  if (progress)
  {
    pbar <- txtProgressBarMulticore(0, length(ctg_clusters), style = 3)
  }

  # Create or clean the temporary directory
  if (savevrt)
  {
    if (!dir.exists(exportdir))
      dir.create(exportdir)
    else
      unlink(exportdir, recursive = TRUE) ; dir.create(exportdir)
  }

  # ========================================
  # Computation over the entire catalog
  # ========================================

  if (numcores == 1)
  {
    verbose("Computing sequentially the metrics for each cluster...")

    output = lapply(ctg_clusters, FUN = apply_grid_func,
                    grid_func = grid_func,
                    catalog   = catalog,
                    res       = res,
                    param     = callparam,
                    save_tiff = savevrt,
                    pb        = pbar,
                    filter    = filter,
                    select    = select)
  }
  else
  {
    verbose("Computing in parallel the metrics for each cluster...")

    cl = parallel::makeCluster(numcores, outfile = "")
    parallel::clusterExport(cl, varlist = c(utils::lsf.str(envir = globalenv()), ls(envir = environment())), envir = environment())
    output = parallel::parLapply(cl, ctg_clusters, fun = apply_grid_func,
                                 grid_func = grid_func,
                                 catalog   = catalog,
                                 res       = res,
                                 param     = callparam,
                                 save_tiff = savevrt,
                                 pb        = pbar,
                                 filter    = filter,
                                 select    = select)
    parallel::stopCluster(cl)
  }

  # Post process of the results (return adequate object)
  if (!savevrt)
  {
    # Return a data.table
    ._class = class(output[[1]])
    output = data.table::rbindlist(output)
    data.table::setattr(output, "class", ._class)
    data.table::setattr(output, "res", res)
  }
  else
  {
    # Build virtual raster mosaic and return it
    ras_lst = list.files(exportdir, full.names = TRUE, pattern = ".tif$")
    save_in = exportdir %+% "/" %+% funcname %+% ".vrt"
    gdalUtils::gdalbuildvrt(ras_lst, save_in)
    output = raster::stack(save_in)
  }

  return(output)
}

# Apply for a given ROI of a catlog a grid_* function
#
# @param X list. the coordinates of the region of interest (rectangular)
# @param grid_func function. the grid_* function to be applied
# @param ctg  LAScatalog.
# @param res numric. the resolution to apply the grid_* function
# @param filter character. the streaming filter to be applied
# @param param list. the parameter of the function grid_function but res
# @param p progressbar.
apply_grid_func = function(ctg_cluster, grid_func, catalog, res, param, save_tiff, pb, filter, select, ...)
{
  X <- Y <- NULL

  # Variables for readability
  xleft   <- ctg_cluster$xleft
  xright  <- ctg_cluster$xright
  ybottom <- ctg_cluster$ybottom
  ytop    <- ctg_cluster$ytop
  name    <- "ROI" %+% ctg_cluster$name
  path    <- ctg_cluster$path
  xcenter <- ctg_cluster$xcenter
  ycenter <- ctg_cluster$ycenter
  width   <- (xright - xleft)/2
  height  <- (ytop - ybottom)/2
  buffer  <- xleft - ctg_cluster$xleftbuff + 0.1*res

  # Update progress bar
  if (!is.null(pb))
    addTxtProgressBarMulticore(pb, 1)

  # Extract the ROI as a LAS object
  las <- catalog_queries_internal(catalog, xcenter, ycenter, width, height, buffer, name, 1, FALSE, filter = filter, select = select)[[1]]

  # Skip if the ROI fall in a void area
  if (is.null(las))
    return(NULL)

  # Because catalog_queries keep point inside the boundingbox (close interval) but point which
  # are exactly on the boundaries are counted twice. Here a post-process to make an open
  # interval on left and bottom edge of the boudingbox.
  # if (buffer == 0)
  # {
  #   n <- fast_countequal(las@data$X, xleft) + fast_countequal(las@data$Y, ybottom)
  #
  #   if (n > 0)
  #     las <- suppressWarnings(lasfilter(las, X = xleft, Y > ybottom))
  #
  #   # Very unprobable but who knows...
  #   if (is.null(las))
  #     return(NULL)
  # }

  # Call the function
  param$x   <- las
  param$res <- res
  metrics   <- do.call(grid_func, args = param)

  # Remove the buffer
  metrics <- metrics[X >= xleft & X <= xright & Y >= ybottom & Y <= ytop]
  as.lasmetrics(metrics, res)

  # Return results or write file
  if (!save_tiff)
  {
    return(metrics)
  }
  else
  {
    if (nrow(metrics) == 0)
      return(NULL)

    metrics <- as.raster(metrics)
    raster::writeRaster(metrics, path, format = "GTiff")
    return(NULL)
  }
}