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


#' Extract LiDAR data from based on a set of coordinates
#'
#' When the user has a set of (x, y) coordinates corresponding to a region of interest (ROI)
#' (a ground inventory for example), he can automatically extract the lidar data associated
#' to the ROIs from a Catalog. The algorithm automatically extracts ROIs even falling on the edges
#' of one or more tiles.\cr
#' It works only for tiles well organized as a damier.
#'
#' @aliases roi_query
#' @param obj A Catalog object
#' @param x vector. A set of x coordinates corresponding to the center of the ROI
#' @param y vector. A set of y coordinates corresponding to the center of the ROI
#' @param r numeric or vector. A radius or a set of radii of the ROI. If only
#' radius is provided (r2 = NULL) it will extract data falling into a disc.
#' @param r2 numeric or vector. A radius or a set of radii of plots. If r2
#' is provided, the selection turns into a rectangular ROI. If r = r2 it is a square obviouly.
#' @param roinames vector. A set of ROI names (the ID of the plots for example)
#' @param ... additionnal parameters for \link[lidR:readLAS]{readLAS}
#' @return A list of LAS objects
#' @export roi_query
#' @importFrom dplyr progress_estimated
#' @importFrom magrittr %>% %<>%
#' @examples
#' \dontrun{
#' # Build a Catalog
#' catalog = Catalog("<Path to a folder containing a set of las or laz files>")
#'
#' # Get coordinates from an external file
#' X = runif(30, 690000, 800000)
#' Y = runif(30, 5010000, 5020000)
#' R = 25
#'
#' # Return a List of 30 circular LAS objects of 25 m radius (50 m diameter)
#' catalog %>% roi_query(X, Y, R)
#'
#' # Return a List of 30 square LAS objects of 50x50 m
#' catalog %>% roi_query(X, Y, R, R)
#' }
setGeneric("roi_query", function(obj, x, y, r, r2 = NULL, roinames = NULL, ...){standardGeneric("roi_query")})

#' @rdname roi_query
setMethod("roi_query", "Catalog",
  function(obj, x, y, r, r2 = NULL, roinames = NULL, ...)
  {
    CIRCLE = 0
    RECTANGLE = 1

    nplot  = length(x)
    output = vector("list", nplot)
    p      = dplyr::progress_estimated(nplot)
    k      = 1
    type   = if(is.null(r2)) CIRCLE else RECTANGLE

    if(is.null(roinames)) roinames = paste("ROI", 1:nplot, sep="")

    names(output) = roinames

    # Make an index of the file in which are each query
    lasindex = obj %>% roi_index(x, y, r, r2)

    # Group the index of idendical queries with the aim to reduce number ofqueries
    lasindex = lasindex[,.(roinames = list(roinames),
                           X = list(x),
                           Y = list(y),
                           r = list(r),
                           r2 = list(r2),
                           tiles=list(unique(unlist(tiles)))),
                        by=list(paste(tiles))][,paste := NULL]

    nqueries = dim(lasindex)[1]

    cat("Extracting data...\n")

    for(i in 1:nqueries)
    {
      query = lasindex[i]

      X     = query$X[[1]]
      Y     = query$Y[[1]]
      r     = query$r[[1]]
      r2    = query$r2[[1]]
      files = query$tiles[[1]]

      lidar = readLAS(files,...)

      for(j in 1:length(X))
      {
        if(type == CIRCLE)
          output[[k]] = clipCircle(lidar, X[j], Y[j], r[j])
        else
          output[[k]] = clipRectangle(lidar, X[j]-r[j], Y[j]-r2[j], X[j]+r[j], Y[j]+r2[j])

        k = k+1
        p$tick()$print()
      }
    }

    cat("\n")

    return(output)
  }
)
