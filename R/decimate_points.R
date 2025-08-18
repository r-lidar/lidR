#' Decimate a LAS object
#'
#' Reduce the number of points using several possible algorithms.
#'
#' @template param-las
#' @param algorithm function. An algorithm of point decimation. \code{lidR} have: \link{random},
#' \link{homogenize}, \link{highest}, \link{lowest}, \link{random_per_voxel} and \link{barycenter_per_voxel}.
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file format
#' and all the attributes. `select = "*"` is imposed internally.\cr
#' The options `chunk buffer` is not supported and not respected because it is not needed.
#'
#' @template return-lasfilter-las-lascatalog
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' las <- readLAS(LASfile, select = "xyz")
#'
#' # Select points randomly to reach an overall density of 1
#' thinned1 <- decimate_points(las, random(1))
#' #plot(rasterize_density(las))
#' #plot(rasterize_density(thinned1))
#'
#' # Select points randomly to reach an homogeneous density of 1
#' thinned2 <- decimate_points(las, homogenize(1,5))
#' #plot(rasterize_density(thinned2))
#'
#' # Select the highest point within each pixel of an overlayed grid
#' thinned3 = decimate_points(las, highest(5))
#' #plot(thinned3)
decimate_points = function(las, algorithm)
{
  UseMethod("decimate_points", las)
}

#' @export
decimate_points.LAS = function(las, algorithm)
{
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dec(algorithm)
  lidR.context <- "decimate_points"
  selected <- algorithm(las)
  return(las[selected])
}

#' @export
decimate_points.LAScatalog = function(las, algorithm)
{
  # Defensive programming
  assert_is_algorithm(algorithm)
  assert_is_algorithm_dec(algorithm)

  # Enforce some options
  opt_select(las) <- "*"
  opt_chunk_buffer(las) <- 0

  e <- environment(algorithm)
  if (!is.null(e[["res"]])) opt_chunk_buffer(las) <- e[["res"]]

  # Processing
  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, decimate_points, algorithm = algorithm, .options = options)
  return(output)
}
