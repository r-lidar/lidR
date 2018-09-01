# ===============================================================================
#
# PROGRAMMERS:
#
# jean-romain.roussel.1@ulaval.ca  -  https:#github.com/Jean-Romain/lidR
#
# COPYRIGHT:
#
# Copyright 2016-2018 Jean-Romain Roussel
#
# This file is part of lidRExtra R package.
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
# along with this program.  If not, see <http:#www.gnu.org/licenses/>
#
# ===============================================================================


#' Classify points as ground or not ground
#'
#' Classify points as ground or not ground with several possible algorithms. The function is a wrapper
#' around all the existing methods. The functions update the field \code{Classification} of the LAS
#' input object. The points classified as 'ground' are assigned a value of 2 according to las
#' specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param algorithm character. The name of an algorithm. Currently \code{"pmf"} and \code{"csf"} are
#' supported (see related sections).
#' @param ... parameters for the algorithms. These depend on the algorithm used (see details
#' of the algorithms).
#'
#' @template  return-lasground
#'
#' @export
#' @family lasground
#' @importFrom data.table :=
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las = readLAS(LASfile, select = "xyzrn")
#'
#' ws = seq(3,12, 3)
#' th = seq(0.1, 1.5, length.out = length(ws))
#'
#' lasground(las, "pmf", ws, th)
#'
#' plot(las, color = "Classification")
lasground = function(las, algorithm,  ...)
{
  if (algorithm == "pmf")
    return(lasground_pmf(las, ...))
  else if (algorithm == "csf")
    return(lasground_csf(las, ...))
  else
    stop("This algorithm does not exist.", call. = F)
}

lasground_generic = function(las, method, last_returns, ...)
{
  UseMethod("lasground_generic", las)
}


lasground_generic.LAS = function(las, method, last_returns, ...)
{
  . <- X <- Y <- Z <- Classification <- NULL

  npoints <- nrow(las@data)
  filter  <- !logical(npoints)
  pointID <- 1:npoints

  if (last_returns)
  {
    n <- names(las@data)

    if (!all(c("ReturnNumber", "NumberOfReturns") %in% n))
      warning("'ReturnNumber' and/or 'NumberOfReturns' not found. Cannot use the option 'last_returns', all the points will be used", call. = FALSE)
    else
      filter = las@data$ReturnNumber == las@data$NumberOfReturns

    if(sum(filter) == 0)
      stop("0 last return found. Process aborted.", call. = FALSE)
  }

  cloud <- las@data[filter, .(X,Y,Z)]
  cloud[, idx := pointID[filter]]

  p = list(...)

  if (method == "pmf")
  {
    for (i in 1:length(p$ws))
    {
      verbose(glue::glue("Pass {i} of {length(p$ws)}..."))
      verbose(glue::glue("Windows size = {p$ws[i]} ; height_threshold = {p$th[i]}"))

      Z_f = C_MorphologicalOpening(cloud$X, cloud$Y, cloud$Z, p$ws[i])

      # Find indices of the points whose difference between the source and
      # filtered point clouds is less than the current height threshold
      diff = cloud$Z - Z_f
      indices = diff < p$th[i]

      # Limit filtering to those points currently considered ground returns
      cloud = cloud[indices]
    }

    idx <- cloud$idx
  }
  else if (method == "csf")
  {
    gnd <- RCSF:::R_CSF(cloud, p$sloop_smooth, p$class_threshold, p$cloth_resolution, p$rigidness, p$iterations, p$time_step)
    idx <- cloud$idx[gnd]
  }
  else
    stop("Internal error in 'lasground_generic'. Please report the issue.")

  message(glue::glue("{length(idx)} ground points found."))

  if ("Classification" %in% names(las@data))
  {
    nground = fast_countequal(las@data$Classification, 2)

    if (nground > 0)
    {
      message(glue::glue("Original dataset already contains {nground} ground points. These points were reclassified as 'unclassified' before to perform a new ground classification."), call. = FALSE)
      las@data[Classification == 2, Classification := 0]
    }
  }
  else
  {
    las@data[, Classification := 0L]
  }

  las@data[idx, Classification := 2L]

  return(invisible(las))
}