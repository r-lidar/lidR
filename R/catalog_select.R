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

#' Select LAS files manually from a LAScatalog
#'
#' Select a set of LAS tiles from a LAScatalog interactively using the mouse. This function
#' allows users to subset a LAScatalog by clicking on a map of the file.
#'
#' @param ctg A \link[lidR:LAScatalog-class]{LAScatalog} object
#'
#' @param mapview logical. If \code{FALSE}, use R base plot instead of mapview (no pan, no zoom, see
#' also \link[lidR:plot]{plot})
#' @param method character. By default selecting tiles that are a subset of the catalog. It is also possible to flag
#' the files to maintain the catalog as a whole but process only a subset of its content.
#' \code{flag_unprocessed} enables users to point and click on files that will not be processed.
#' \code{flag_processed} enables users to point and click on files that will be processed.
#'
#' @return A LAScatalog object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ctg = readLAScatalog("<Path to a folder containing a set of .las files>")
#' new_ctg = catalog_select(ctg)
#' }
# nocov start
catalog_select = function(ctg, mapview = TRUE, method = c("subset", "flag_unprocessed", "flag_processed"))
{
  assert_is_all_of(ctg, "LAScatalog")
  assert_is_a_bool(mapview)
  method <- match.arg(method)

  Min.X <- Min.Y <- Max.X <- Max.Y <- NULL

  if (mapview & (!requireNamespace("mapview", quietly = TRUE) | !requireNamespace("mapedit", quietly = TRUE)))
  {
    message("Package 'mapview' and 'mapedit' are needed. Function switched to R base plot mode.")
    mapview = FALSE
  }

  if (mapview)
  {
    index <- mapedit::selectFeatures(as.spatial(ctg),index = TRUE)
  }
  else
  {
    plot(ctg, mapview = FALSE)
    index <- with(ctg@data, identify_tile(Min.X, Max.X, Min.Y, Max.Y))
  }

  if (method == "subset") {
    ctg <- ctg[index,]
  } else if (method == "flag_unprocessed") {
    ctg$processed <- TRUE
    ctg$processed[index] <- FALSE
  } else {
    ctg$processed <- FALSE
    ctg$processed[index] <- TRUE
  }

  return(ctg)
}

identify_tile <- function(minx, maxx, miny, maxy, plot = FALSE, ...)
{
  n <- length(minx)
  x <- (minx + maxx)/2
  y <- (miny + maxy)/2

  sel <- rep(FALSE, n)

  while (sum(sel) < n)
  {
    ans <- graphics::identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)

    if (!length(ans))
      break

    ans <- which(!sel)[ans]

    graphics::rect(minx[ans], miny[ans], maxx[ans], maxy[ans], col = "forestgreen")

    sel[ans] <- TRUE
  }

  return(which(sel))
}
# nocov end

