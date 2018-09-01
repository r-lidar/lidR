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
#' Classify points as ground or not ground with several possible algorithms. The function updates the
#' attribute \code{Classification} of the LAS object. The points classified as 'ground' are assigned
#' a value of 2 according to las' specifications (See the ASPRS documentation for the
#' \href{http://www.asprs.org/a/society/committees/standards/LAS_1_4_r13.pdf}{LAS file format}).
#'
#' @template LAScatalog
#'
#' @template section-supported-option-lasupdater
#'
#' @template param-las
#' @param algorithm a ground segmentation function. lidR have two compatible functions: \link{pmf} and \link{csf}
#' @param last_returns logical. The algorithm will use only the last returns (including the first returns
#' in the cases of single return) to run the algorithm. If FALSE all the returns are used. If the attribute
#' \code{'ReturnNumber'} or \code{'NumberOfReturns'} are not specified \code{'last_returns'} is turned
#' to \code{FALSE} automatically.
#'
#' @return If the input is a \code{LAS} object, returns nothing. The original LAS object is updated by
#' reference. The 'Classification' attributes have a value of 2 that denotes 'ground' according to LAS
#' specifications.\cr\cr
#' If the input is a \code{LAScatalog} returns a \code{LAScatalog}.
#'
#' @export
#' @family Ground Segmentation
#' @importFrom data.table :=
#' @examples
#' LASfile <- system.file("extdata", "Topography.laz", package="lidR")
#'
#' # Using the Progressive Morphological Filter
#' # --------------------------------------
#'
#' las = readLAS(LASfile, select = "xyzrn")
#'
#' ws = seq(3,12, 3)
#' th = seq(0.1, 1.5, length.out = length(ws))
#'
#' lasground(las, pmf(ws, th))
#' plot(las, color = "Classification")
#'
#' # Using the Cloth Simulation Filter
#' # --------------------------------------
#'
#' las = readLAS(LASfile, select = "xyzrn")
#'
#' lasground(las, csf())
lasground = function(las, algorithm, last_returns = TRUE)
{
  UseMethod("lasground", las)
}

#' @export
lasground.LAS = function(las, algorithm, last_returns = TRUE)
{
  if (!is(algorithm, "lidR") | !is(algorithm, "Algorithm"))
    stop("Invalid function provided as algorithm.", call. = FALSE)

  if (!is(algorithm, "GroundSegmentation"))
    stop("The algorithm is not an algorithm for ground segmentation", call. = FALSE)

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

  lidR.context <- "lasground"
  idx <- algorithm(cloud)

  #message(glue::glue("{length(idx)} ground points found."))

  if ("Classification" %in% names(las@data))
  {
    nground = fast_countequal(las@data$Classification, 2L)

    if (nground > 0)
    {
      message(glue::glue("Original dataset already contains {nground} ground points. These points were reclassified as 'unclassified' before to perform a new ground classification."))
      las@data[Classification == LASGROUND, Classification := LASUNCLASSIFIED]
    }
  }
  else
  {
    las@data[, Classification := LASUNCLASSIFIED]
  }

  las@data[idx, Classification := LASGROUND]

  return(invisible(las))
}

#' @export
lasground.LAScluster = function(las, algorithm, last_returns = TRUE)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  lasground(x, algorithm, last_returns)
  x <- lasfilter(x, buffer == LIDRNOBUFFER)
  return(x)
}

#' @export
lasground.LAScatalog = function(las, algorithm, last_returns = TRUE)
{
  set_select(las) <- "*"

  output <- catalog_apply2(las, lasground, algorithm = algorithm, last_returns = last_returns,  need_buffer = TRUE, check_alignement = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output <- unlist(output)
  ctg    <- catalog(output)
  ctg@proj4string <- las@proj4string
  return(ctg)
}