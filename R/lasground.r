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
# along with this program.  If not, see <http:#www.gnu.org/licenses/>
#
# ===============================================================================


#' Classify points as 'ground' or 'not ground'
#'
#' Classify points as 'ground' or 'not ground' with several possible algorithms. The function updates the
#' attribute \code{Classification} of the LAS object. The points classified as 'ground' are assigned
#' a value of 2 according to \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{las specifications}.
#'
#' @template param-las
#'
#' @param algorithm a ground-segmentation function. \code{lidR} has: \link{pmf} and \link{csf}.
#' @param last_returns logical. The algorithm will use only the last returns (including the first returns
#' in cases of a single return) to run the algorithm. If FALSE all the returns are used. If the attribute
#' \code{'ReturnNumber'} or \code{'NumberOfReturns'} are absent, \code{'last_returns'} is turned
#' to \code{FALSE} automatically.
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template return-lasupdater-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyzrn")
#'
#' # Using the Progressive Morphological Filter
#' # --------------------------------------
#'
#' ws  <- seq(3,12, 3)
#' th  <- seq(0.1, 1.5, length.out = length(ws))
#'
#' las <- lasground(las, pmf(ws, th))
#' plot(las, color = "Classification")
#'
#' # Using the Cloth Simulation Filter
#' # --------------------------------------
#'
#' las <- lasground(las, csf())
lasground = function(las, algorithm, last_returns = TRUE)
{
  UseMethod("lasground", las)
}

#' @export
lasground.LAS = function(las, algorithm, last_returns = TRUE)
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.")

  if (!is(algorithm, "GroundSegmentation"))
    stop("The algorithm is not an algorithm for ground segmentation")

  . <- X <- Y <- Z <- Classification <- NULL

  npoints <- nrow(las@data)
  filter  <- !logical(npoints)
  pointID <- 1:npoints

  cloud <- coordinates3D(las)
  data.table::setDT(cloud)
  cloud[, idx := pointID]

  if (last_returns)
  {
    n <- names(las@data)

    if (!all(c("ReturnNumber", "NumberOfReturns") %in% n))
    {
      warning("'ReturnNumber' and/or 'NumberOfReturns' not found. Cannot use the option 'last_returns', all the points will be used.")
    }
    else
    {
      filter <- las@data$ReturnNumber == las@data$NumberOfReturns

      if (sum(filter) == 0)
        warning("Zero last return found. Cannot use the option 'last_returns', all the points will be used.")
      else
        cloud <- cloud[filter]
    }
  }

  lidR.context <- "lasground"
  idx <- algorithm(cloud)

  #message(glue::glue("{length(idx)} ground points found."))

  if ("Classification" %in% names(las@data))
  {
    nground <- fast_countequal(las@data[["Classification"]], 2L)

    if (nground > 0)
    {
      message(glue::glue("Original dataset already contains {nground} ground points. These points were reclassified as 'unclassified' before performing a new ground classification."))
      new_classes <- las@data[["Classification"]]
      new_classes[new_classes == LASGROUND] <- LASUNCLASSIFIED
    }
    else
    {
      new_classes <- rep(LASUNCLASSIFIED, npoints)
    }
  }
  else
    new_classes <- rep(LASUNCLASSIFIED, npoints)

  new_classes[idx] <- LASGROUND
  las@data[["Classification"]] <- new_classes
  return(las)
}

#' @export
lasground.LAScluster = function(las, algorithm, last_returns = TRUE)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- lasground(x, algorithm, last_returns)
  x <- lasfilter(x, buffer == LIDRNOBUFFER)
  return(x)
}

#' @export
lasground.LAScatalog = function(las, algorithm, last_returns = TRUE)
{
  opt_select(las) <- "*"

  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_apply(las, lasground, algorithm = algorithm, last_returns = last_returns, .options = options)
  output  <- unlist(output)
  ctg     <- catalog(output)

  opt_copy(ctg) <- las
  return(ctg)
}
