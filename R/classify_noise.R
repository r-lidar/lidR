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


#' Classify points as 'noise'
#'
#' Classify points as 'noise' (outliers) with several possible algorithms. The function updates the
#' attribute \code{Classification} of the LAS object. The points classified as 'noise' are assigned
#' a value of 18 according to \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{las specifications}.
#'
#' @template param-las
#'
#' @param algorithm a noise-segmentation function. \code{lidR} has: \link{sor}, \link{ivf}.
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
#' las <- readLAS(LASfile, filter = "-inside 273450 5274350 273550 5274450")
#'
#' # Add 20 artificial outliers
#' set.seed(314)
#' id = round(runif(20, 0, npoints(las)))
#' set.seed(42)
#' err = runif(20, -50, 50)
#' las$Z[id] = las$Z[id] + err
#'
#' # Using SOR
#' las <- classify_noise(las, sor(15,7))
#' #plot(las, color = "Classification")
#'
#' # Using IVF
#' las <- classify_noise(las, ivf(5,2))
#'
#' # Remove outliers using filter_poi()
#' las_denoise <- filter_poi(las, Classification != LASNOISE)
classify_noise = function(las, algorithm)
{
  UseMethod("classify_noise", las)
}

#' @export
classify_noise.LAS = function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_out(algorithm)

  lidR.context <- "classify_noise"
  idx <- algorithm(las)

  if ("Classification" %in% names(las@data))
  {
    nnoise <- fast_countequal(las@data[["Classification"]], LASNOISE)

    if (nnoise > 0)
    {
      message(glue::glue("Original dataset already contains {nnoise} noise points. These points were reclassified as 'unclassified' before performing a new noise classification."))
      new_classes <- las@data[["Classification"]]
      new_classes[new_classes == LASNOISE] <- LASUNCLASSIFIED
    }
    else
    {
      new_classes <- las@data[["Classification"]]
    }
  }
  else
    new_classes <- rep(LASUNCLASSIFIED, npoints(las))

  new_classes[idx] <- LASNOISE
  las@data[["Classification"]] <- new_classes
  return(las)
}

#' @export
classify_noise.LAScluster = function(las, algorithm)
{
  buffer <- NULL
  x <- readLAS(las)
  if (is.empty(x)) return(NULL)
  x <- classify_noise(x, algorithm)
  x <- filter_poi(x, buffer == LIDRNOBUFFER)
  return(x)
}

#' @export
classify_noise.LAScatalog = function(las, algorithm)
{
  opt_select(las) <- "*"
  options <- list(need_buffer = TRUE, drop_null = TRUE, need_output_file = TRUE, automerge = TRUE)
  output  <- catalog_apply(las, classify_noise, algorithm = algorithm, .options = options)
  return(output)
}
