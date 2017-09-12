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


#' Extract LiDAR data based on a set of coordinates
#'
#' From a set of (x, y) coordinates corresponding to the centers of regions of interest (ROIs),
#' for example a ground inventory, the function automatically extracts the lidar data associated
#' with the ROIs from a \link{catalog}. The algorithm will do this even for ROIs falling on
#' the edges of one or more tiles. The extracted lidar data can be buffered. In this case the
#' function adds a buffer area around the ROIs, and the LAS object returned has an extra column
#' named '\code{buffer}' which indicates, for each point, if the point is in the buffer or
#' from the ROI (see more in the section Buffer).\cr\cr
#' \code{lidR} support .lax file. You will speed-up the computation \emph{a lot} with a spatial
#' index.
#'
#' @section Buffer:
#' If the ROIs are buffered then the LAS objects returned by the function have extra points.
#' The LAS objects received by the user contain a special column called 'buffer', which indicates,
#' for each point, if it comes from a buffered area or not. Points from non-buffered areas (i.e.
#' the ROI) have a 'buffer' value of 0, while those from buffered areas have a 'buffer' value
#' greater than 0.\cr\cr
#' For a circular ROI, points in the buffered area have a buffer value of 1. For a rectangular
#' ROI the points in the buffer area have a buffer value of 1, 2, 3 or 4, where 1 is the bottom
#' buffer and 2, 3 and 4 are the left, top and right buffers, respectively.
#'
#' @section Multicore computation:
#' The process is done using several cores. To change the settings of how a catalog is processed
#' use \link{catalog_options}.
#'
#' @param obj A LAScatalog object
#' @param x vector. A set of x coordinates corresponding to the centers of the ROIs
#' @param y vector. A set of y coordinates corresponding to the centers of the ROIs
#' @param r numeric or vector. A radius or a set of radii of the ROIs. If only
#' r is provided (r2 = NULL) it will extract data falling onto a disc.
#' @param r2 numeric or vector. A radius or a set of radii of plots. If r2
#' is provided, the selection turns into a rectangular ROI (if r = r2 it is a square).
#' @param buffer numeric. Adds a buffer area around the ROI. See relevant sections.
#' @param roinames vector. A set of ROI names (the plot IDs, for example) to label the
#' returned list.
#' @param ... Any argument available in \link{readLAS} to reduce the amount of data loaded.
#' @return A list of LAS objects
#' @seealso
#' \link{readLAS}
#' \link{catalog}
#' \link{catalog_options}
#' @export
#' @examples
#' \dontrun{
#' # Build a LAScatalog
#' catalog = catalog("<Path to a folder containing a set of .las or .laz files>")
#'
#' # Get coordinates from an external file
#' X = runif(30, 690000, 800000)
#' Y = runif(30, 5010000, 5020000)
#' R = 25
#'
#' # Return a List of 30 circular LAS objects of 25 m radius
#' catalog %>% catalog_queries(X, Y, R)
#'
#' # Return a List of 30 square LAS objects of 50x50 m
#' catalog %>% catalog_queries(X, Y, R, R)
#'
#' # Return a List of 30 circular LAS objects of 30 m radius. 25 m being the ROI and 5 m
#' # being a buffered area. The LAS objects have an extra column called 'buffer' to
#' # differentiate the points.
#' catalog %>% catalog_queries(X, Y, R, buffer = 5)
#'
#' # Return a List of 30 circular LAS objects of 25 m radius for which only the fields X, Y and
#' # Z have been loaded and Z values < 0 were removed.
#' catalog %>% catalog_queries(X, Y, R, XYZonly = TRUE, filter = "-drop_z_below 0")
#' }
catalog_queries = function(obj, x, y, r, r2 = NULL, buffer = 0, roinames = NULL, ...)
{
  UseMethod("catalog_queries", obj)
}

#' @export
catalog_queries.LAScatalog = function(obj, x, y, r, r2 = NULL, buffer = 0, roinames = NULL, ...)
{
  objtxt = lazyeval::expr_text(obj)
  xtxt   = lazyeval::expr_text(x)
  ytxt   = lazyeval::expr_text(y)
  rtxt   = lazyeval::expr_text(r)
  btxt   = lazyeval::expr_text(buffer)

  if (length(x) != length(y))
    stop(paste0(xtxt, " is not same length as ", ytxt), call. = FALSE)

  if (length(r) > 1 & (length(x) != length(r)))
    stop(paste0(xtxt, " is not same length as ", rtxt), call. = FALSE)

  if (length(buffer) > 1 & (length(x) != length(buffer)))
    stop(paste0(xtxt, " is not same length as ", btxt), call. = FALSE)

  if (any(buffer < 0))
    stop("Buffer size must be a positive value", call. = FALSE)

  ncores   = CATALOGOPTIONS("multicore")
  progress = CATALOGOPTIONS("progress")

  output = catalog_queries_internal(obj, x, y, r, r2, buffer, roinames, ncores, progress, ...)

  return(output)
}

catalog_queries_internal = function(obj, x, y, r, r2, buffer, roinames, ncores, progress, ...)
{
  nplots <- length(x)
  tiles  <- pbar <- NULL

  if (is.null(roinames))
    roinames <- paste0("ROI", 1:nplots)

  verbose("Indexing files...")

  # Make an index of the files containing each query
  queries = catalog_index(obj, x, y, r, r2, buffer, roinames)
  nplots  = length(queries)

  if (progress)
    pbar  = txtProgressBarMulticore(min = 0, max = nplots, style = 3)

  if (nplots <= ncores)
    ncores = nplots

  verbose("Extracting data...")

  # Computation
  if (ncores == 1)
  {
    output = sapply(queries, .get_query, pb = pbar, ..., simplify = FALSE, USE.NAMES = TRUE)
  }
  else
  {
    cl = parallel::makeCluster(ncores, outfile = "")
    parallel::clusterExport(cl, varlist = NULL, envir = NULL)
    output = parallel::parSapply(cl, queries, .get_query, pb = pbar, ..., simplify = FALSE, USE.NAMES = TRUE)
    parallel::stopCluster(cl)

    # This patch solves issue #73 in a dirty way waiting for a better solution for issue
    # 2333 in data.table
    for (i in 1:length(output))
      output[[i]]@data <- data.table::alloc.col(output[[i]]@data)
  }

  return(output)
}

.get_query = function(query, pb = NULL, ...)
{
  X <- Y <- buffer <- NULL

  # Variables for readability
  x       <- query$x
  y       <- query$y
  r       <- query$r
  r2      <- query$r2
  buff    <- query$buffer
  tiles   <- query$tiles
  shape   <- query$shape

  xleft   <- x - r
  xright  <- x + r
  ybottom <- y - r2
  ytop    <- y + r2

  select  <- "*"
  param   <- list(...)

  if (shape == LIDRCIRCLE)
    filter <- paste("-inside_circle", x, y, r)
  else if (shape == LIDRRECTANGLE)
    filter <- paste("-inside", xleft, ybottom, xright, ytop)
  else
    stop("Something went wrong internally in .get_query(). Process aborted.")

  if (!is.null(pb))
    addTxtProgressBarMulticore(pb, 1)

  # Merge spatial filter with user's filters
  if (!is.null(param$filter))
    filter <- paste(filter, param$filter)

  if (!is.null(param$select))
    select <- param$select

  las <- readLAS(tiles, filter = filter, select = select)

  if (is.null(las))
    return(NULL)

  if (buff == 0)
    return(las)

  las@data[, buffer := 0]

  if (shape == LIDRCIRCLE)
  {
    las@data[(X-x)^2 + (Y-y)^2 > (r-buff)^2, buffer := LIDRBUFFER]
  }
  else
  {
    las@data[Y < ybottom + buff, buffer := LIDRBOTTOMBUFFER]
    las@data[X < xleft   + buff, buffer := LIDRLEFTBUFFER]
    las@data[Y > ytop    - buff, buffer := LIDRTOPBUFFER]
    las@data[X > xright  - buff, buffer := LIDRRIGHTBUFFER]
    las@data[(X > xright - buff) & (Y < ybottom + buff), buffer := LIDRBOTTOMBUFFER]
  }

  return(las)
}

