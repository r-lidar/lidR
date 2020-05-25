# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https://github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2017 Jean-Romain Roussel
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

#' Subdivide a LAScatalog into chunks
#'
#' Virtually subdivide a LAScatalog into chunks. This function is an internal function exported to
#' users in version 3.0.0 because it might be useful for some debugging purposes. It might also be useful for
#' some advanced developers. Regular users are not expected to use this function. The chunks are made
#' according to the \link[lidR:catalog_options_tools]{catalog processing options}.
#'
#' @param ctg an object of class \code{LAScatalog}
#' @param realignment \code{FALSE} or \code{list(res = x, start = c(y,z))}. Sometimes the chunk must
#' be aligned with a raster, for example to ensure the continuity of the output. If the chunk size is
#' 800 and the expected product is a raster with a resolution of 35, 800 and 35 are not compatible
#' and will create 2 different partial pixels on the edges. The realignment option forces the
#' chunk to fit the grid aligment.
#' @param plot logical. Displays the chunk pattern.
#'
#' @return A list containing objects of class \code{LAScluster}.
#'
#' @export
catalog_makechunks = function(ctg, realignment = FALSE, plot = opt_progress(ctg))
{
  assert_is_all_of(ctg, "LAScatalog")

  # Bounding box of the catalog
  xmin <- ymin <- xmax <- ymax <- 0

  # Put some options in variables with short names
  buffer <- opt_chunk_buffer(ctg)
  by_file <- opt_chunk_is_file(ctg)
  chunk_alignment <- opt_chunk_alignment(ctg)
  width <- opt_chunk_size(ctg)
  realign <- !isFALSE(realignment) && opt_wall_to_wall(ctg)
  realigned <- FALSE

  # New feature from v2.2.0 to not process some tiles
  processed <- ctg@data$processed
  if (is.null(processed)) processed <- rep(TRUE, nrow(ctg@data))
  if (!is.logical(processed)) stop("The attribute 'processed' of the catalog is not logical.", call. = FALSE)
  files <- ctg@data$filename[processed]

  if (by_file)
  {
    # If processing by file the chunks are easy to make. It corresponds
    # to the bounding boxes of the files
    xmin <- ctg@data[["Min.X"]]
    xmax <- ctg@data[["Max.X"]]
    ymin <- ctg@data[["Min.Y"]]
    ymax <- ctg@data[["Max.Y"]]

    # Remove the files that are flagged for buffer only but no actual processing
    xmin <- xmin[processed]
    xmax <- xmax[processed]
    ymin <- ymin[processed]
    ymax <- ymax[processed]

    # Realignment happens when the chunks need a specific alignment (e.g. with a raster)
    # If a raster has a resolution of 16 m and the files are 1000 x 1000 meters
    # the file pattern implies the edge pixels will be split into two equal parts. A size of 1000 is
    # thus not valid and must be resized to 1008 to fit with the grid.
    if (realign)
    {
      res <- realignment$res
      xscale <- ctg[["X.scale.factor"]][processed]
      yscale <- ctg[["Y.scale.factor"]][processed]

      new_xmin <- round_any(xmin, res)
      new_ymin <- round_any(ymin, res)
      new_xmax <- round_any(xmax, res)
      new_ymax <- round_any(ymax, res)

      resize_xmin = new_xmin < xmin - xscale | new_xmin > xmin + xscale
      resize_ymin = new_ymin < ymin - yscale | new_ymin > ymin + yscale
      resize_xmax = new_xmax < xmax - xscale | new_xmax > xmax + xscale
      resize_ymax = new_ymax < ymax - yscale | new_ymax > ymax + yscale

      if (any(resize_xmin) || any(resize_ymin) || any(resize_xmax) || any(resize_ymax))
      {
        xmin[resize_xmin] <- new_xmin[resize_xmin] - ifelse(new_xmin[resize_xmin] >= xmin[resize_xmin], res, 0)
        ymin[resize_ymin] <- new_ymin[resize_ymin] - ifelse(new_ymin[resize_ymin] >= ymin[resize_ymin], res, 0)
        xmax[resize_xmax] <- new_xmax[resize_xmax] + ifelse(new_xmax[resize_xmax] <= xmax[resize_xmax], res, 0)
        ymax[resize_ymax] <- new_ymax[resize_ymax] + ifelse(new_ymax[resize_ymax] <= ymax[resize_ymax], res, 0)

        ns <- sum(resize_xmin | resize_ymin | resize_xmax | resize_ymax)
        nt <- length(resize_xmin)
        # message(glue::glue("The tiling pattern does not match with the resolution {res}. {ns}/{nt} chunks were extended to avoid partial pixels."))
        realigned <- TRUE
      }
    }
  }
  else
  {
    # Realignment happens when the chunks need a specific alignment (e.g. with a raster)
    # If a raster has a resolution of 16 m and the chunk are 500 x 500 meters
    # the chunk pattern implies the edge pixels will be split into two unequal parts.
    # A size of 500 is thus not valid and must be resized to 512 to fit with the grid.
    if (realign)
    {
      res <- realignment$res
      start <- realignment$start

      # If the chunk_size option does not match with the resolution
      new_width <- round_any(width, res)

      if (new_width != width)
      {
        message(glue::glue("Chunk size does not match with the resolution of the raster. Chunk size changed to {new_width} instead of {width} to ensure continuity of the output."))
        width <- new_width
        realigned <- TRUE
      }

      # If the alignment of the chunks does not match the start point of the raster
      new_chunk_alignment <- abs((chunk_alignment - start)) %% res + chunk_alignment
      if (any(new_chunk_alignment != chunk_alignment))
      {
        message(glue::glue("Alignment of the chunks does not match with the starting points of the raster. Alignment changed to ({new_chunk_alignment[1]}, {new_chunk_alignment[2]}) to ensure continuity of the output."))
        chunk_alignment <- new_chunk_alignment
        realigned <- TRUE
      }
    }

    # Bounding box of the catalog
    bbox <- with(ctg@data, c(min(Min.X), min(Min.Y), max(Max.X), max(Max.Y)))

    # Shift to align the grid
    shift <- numeric(2)
    shift[1] <- (bbox[1] - chunk_alignment[1]) %% width
    shift[2] <- (bbox[2] - chunk_alignment[2]) %% width

    # Generate coordinates of bottom-left corner
    xmin <- seq(bbox[1] - shift[1], bbox[3], width)
    ymin <- seq(bbox[2] - shift[2], bbox[4], width)
    grid <- expand.grid(xmin = xmin, ymin = ymin)

    xmin <- grid$xmin
    ymin <- grid$ymin
    xmax <- xmin + width
    ymax <- ymin + width
  }
  verbose("Creating a set of clusters for the catalog...")

  # Generate center and width of each chunk
  xcenter <- (xmin + xmax)/2
  ycenter <- (ymin + ymax)/2
  width   <- xmax - xmin
  height  <- ymax - ymin

  # Creation of a set of LASclusters from the rectangles
  if (by_file && buffer <= 0 && realigned == FALSE)
  {
    filenames <- ctg@data$filename[processed]

    clusters = lapply(1:length(xcenter), function(i)
    {
      center  <- list(x = xcenter[i], y = ycenter[i])
      cluster <- LAScluster(center, width[i], height[i], buffer, LIDRRECTANGLE, filenames[i], "noname", proj4string = ctg@proj4string)

      cluster@select <- ctg@input_options$select
      cluster@filter <- paste(cluster@filter, ctg@input_options$filter)

      return(cluster)
    })
  }
  else
  {
    bboxes = mapply(raster::extent, xcenter - width/2, xcenter + width/2, ycenter - height/2, ycenter + height/2)
    clusters = suppressWarnings(catalog_index(ctg, bboxes, LIDRRECTANGLE, buffer, processed, TRUE, by_file))
    clusters = clusters[!sapply(clusters, is.null)]
  }

  # Post-process the clusters

  # Specific optimisation to remove the "-inside" filter
  if (by_file && buffer == 0 && realigned == FALSE)
  {
    clusters <- lapply(clusters, function(x)
    {
      x@filter <- ctg@input_options$filter
      return(x)
    })
  }

  # Record the path indicating where to write the raster if requested
  if (opt_output_files(ctg) != "")
  {
    clusters <- lapply(seq_along(clusters), function(i)
    {
      X         <- list()
      X$ID      <- i
      X$XCENTER <- format(clusters[[i]]@center$x, scientific = F)
      X$YCENTER <- format(clusters[[i]]@center$y, scientific = F)
      X$XLEFT   <- format(clusters[[i]]@bbox[1], scientific = F)
      X$XRIGHT  <- format(clusters[[i]]@bbox[3], scientific = F)
      X$YBOTTOM <- format(clusters[[i]]@bbox[2], scientific = F)
      X$YTOP    <- format(clusters[[i]]@bbox[4], scientific = F)

      usefilename <- grepl("\\{ORIGINALFILENAME\\}",  opt_output_files(ctg))

      if (usefilename && !by_file)
        stop("The template {ORIGINALFILENAME} makes sense only when processing by file (chunk size = 0). It is undefined otherwise.", call. = FALSE)

      if (by_file)
        X$ORIGINALFILENAME <- tools::file_path_sans_ext(basename(files[i]))

      filepath  <- paste0(glue::glue_data(X, opt_output_files(ctg)))
      n         <- length(filepath)

      if (n > 1)
        stop(glue::glue("Ill-formed template string in the catalog: {n} filenames were generated for each chunk"), call. = FALSE)

      clusters[[i]]@save <- filepath
      return(clusters[[i]])
    })
  }

  # Record the alternative directories for remote computation
  if (ctg@input_options[["alt_dir"]][1] != "")
  {
    clusters <- lapply(seq_along(clusters), function(i)
    {
      clusters[[i]]@alt_dir <- ctg@input_options[["alt_dir"]]
      return(clusters[[i]])
    })
  }

  # Plot the catalog and the clusters if progress
  if (plot)
  {
    xrange = c(min(xmin, ctg@data[["Min.X"]]), max(xmax, ctg@data[["Max.X"]]))
    yrange = c(min(ymin, ctg@data[["Min.Y"]]), max(ymax, ctg@data[["Max.Y"]]))
    title  = "Pattern of chunks"
    plot.LAScatalog(ctg, mapview = FALSE, chunk_pattern = FALSE, main = title, xlim = xrange, ylim = yrange)

    lapply(clusters, function(x)
    {
      graphics::rect(x@bbox[1], x@bbox[2], x@bbox[3], x@bbox[4], border = "red")

      if (x@buffer != 0)
        graphics::rect(x@bbbox[1], x@bbbox[2], x@bbbox[3], x@bbbox[4], border = "darkgreen", lty = "dotted")
    })
  }

  return(clusters)
}

catalog_makecluster = catalog_makechunks
