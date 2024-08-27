#' A set of boolean tests on objects
#'
#' \code{is.empty} tests if a \code{LAS} object is a point cloud with 0 points.\cr
#' \code{is.overlapping} tests if a \code{LAScatalog} has overlapping tiles.\cr
#' \code{is.indexed} tests if the points of a \code{LAScatalog} are indexed with \code{.lax} files.\cr
#' \code{is.algorithm} tests if an object is an algorithm of the lidR package.\cr
#' \code{is.parallelised} tests if an algorithm of the lidR package is natively parallelised with OpenMP.
#' Returns TRUE if the algorithm is at least partially parallelised i.e. if some portion of the code is
#' computed in parallel.
#'
#' @param x  \code{LAS} object or any R object.
#' @param catalog A \code{LAScatalog} object.
#' @param algorithm An \code{algorithm} object.
#'
#' @return TRUE or FALSE
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#' is.empty(las)
#'
#' las = new("LAS")
#' is.empty(las)
#'
#' f <- lmf(2)
#' is.parallelised(f)
#'
#' g <- pitfree()
#' is.parallelised(g)
#'
#' ctg <- readLAScatalog(LASfile)
#' is.indexed(ctg)
#' @export
#' @rdname is
#' @name is
NULL

#' @importFrom terra is.empty
#' @rdname is
#' @export
setMethod("is.empty", "LAS", function(x)
{
  stopifnotlas(x)
  return(nrow(x@data) == 0L)
})

#' @rdname is
#' @export
is.overlapping = function(catalog)
{
  sfdf          <- catalog@data
  contour       <- sf::st_union(sfdf)
  actual_area   <- sf::st_area(contour)
  average_area  <- actual_area / length(sfdf)
  measured_area <- sum(sf::st_area(sfdf))
  actual_area   <- actual_area + 0.0000001 * average_area # fix #310
  return(actual_area < measured_area)
}

#' @rdname is
#' @export
is.indexed = function(catalog)
{
  laxfiles <- paste0(tools::file_path_sans_ext(catalog@data$filename), ".lax")
  return(!any(!file.exists(laxfiles)))
}

#' @rdname is
#' @export
is.algorithm = function(x)
{
  return(is(x, LIDRALGORITHM))
}

#' @rdname is
#' @export
is.parallelised = function(algorithm)
{
  if (!is.algorithm(algorithm))
    stop("This function only applies to algorithms from the lidR package")

  return(is(algorithm, LIDRALGORITHMOPENMP))
}

is_raster <- function(raster)
{
  raster_is_supported(raster)
}

