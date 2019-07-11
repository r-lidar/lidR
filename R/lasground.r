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
#' @param filter formula of logical predicates. Enable the functon to run only on points of interest
#' in an optimized way. See also examples. A pertinent conditional statement in \code{lasground}
#' is \code{~ReturnNumber == NumberOfReturns} to compute only on last returns.
#' @param ... unused
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
#' # Using the Cloth Simulation Filter
#' # --------------------------------------
#'
#' las <- lasground(las, csf())
#' plot(las, color = "Classification")
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
#' \dontrun{
#' # Some examples with the filter argument
#' # --------------------------------------
#'
#' # Use all returns instead of last returns by default
#' las <- lasground(las, csf(), filter = NULL)
#'
#' # Use only first returns. This is technically possible but meaningless
#' las <- lasground(las, csf(), filter = ~ReturnNumber == 1L)
#' plot(las, color = "Classification")
#'
#' # Use only returns with an intensity above a threshold (why not?)
#' las <- lasground(las, csf(), filter = ~Intensity > 250L)
#' }
lasground = function(las, algorithm, filter = ~ReturnNumber == NumberOfReturns, ...)
{
  UseMethod("lasground", las)
}

#' @export
lasground.LAS = function(las, algorithm, filter = ~ReturnNumber == NumberOfReturns, ...)
{
  filter <- .last_returns_retro_compatibility(filter, ...)

  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.")

  if (!is(algorithm, "GroundSegmentation"))
    stop("The algorithm is not an algorithm for ground segmentation")

  npoints <- nrow(las@data)
  pointID <- 1:npoints
  cloud   <- coordinates3D(las)
  cloud[, idx := pointID]

  if (!is.null(filter))
  {
    filter <- lasfilter_(las, list(filter))

    if (sum(filter) == 0)
    {
      warning("Zero points found with the filter predicates: all points were used.")
      filter = NULL
    }
  }

  lidR.context <- "lasground"
  idx <- algorithm(cloud, filter)

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
lasground.LAScluster = function(las, algorithm, filter = ~ReturnNumber == NumberOfReturns, ...)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- lasground(x, algorithm, filter)
  x <- lasfilter(x, buffer == LIDRNOBUFFER)
  return(x)
}

#' @export
lasground.LAScatalog = function(las, algorithm, filter = ~ReturnNumber == NumberOfReturns, ...)
{
  filter <- .last_returns_retro_compatibility(filter, ...)

  opt_select(las) <- "*"
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_apply(las, lasground, algorithm = algorithm, filter = filter, .options = options)
  output  <- unlist(output)
  ctg     <- readLAScatalog(output)

  opt_copy(ctg) <- las
  return(ctg)
}

# Retro compatibility with lidR 2.0.y
.last_returns_retro_compatibility = function(filter, ...)
{
  dots <- list(...)

  if (is.logical(filter))
  {
    if (filter)
      return(~ReturnNumber == NumberOfReturns)

    return(NULL)
  }

  if (!is.null(dots$last_returns) && dots$last_returns == TRUE)
    return(~ReturnNumber == NumberOfReturns)

  return(filter)
}
