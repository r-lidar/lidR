#' Filter points of interest
#'
#' Filter points of interest (POI) from a LAS object where conditions are true.
#'
#' \itemize{
#' \item{`filter_poi` Select points of interest based on custom logical criteria.}
#' \item{`filter_first` Select only the first returns.}
#' \item{`filter_firstlast` Select only the first and last returns.}
#' \item{`filter_ground` Select only the returns classified as ground according to LAS specification.}
#' \item{`filter_last` Select only the last returns i.e. the last returns and the single returns.}
#' \item{`filter_nth` Select the returns from their position in the return sequence.}
#' \item{`filter_firstofmany` Select only the first returns from pulses which returned multiple points.}
#' \item{`filter_single` Select only the returns that return only one point.}
#' \item{`filter_duplicates` **Removes** the duplicated points (duplicated by XYZ)}
#' }
#'
#' @section Non-supported LAScatalog options:
#' The option `select` is not supported and not respected because it always preserves the file format
#' and all the attributes. `select = "*"` is imposed internally.
#'
#' @param las An object of class \link[=LAS-class]{LAS}
#' @param n integer  ReturnNumber == n
#' @param \dots Logical predicates. Multiple conditions are combined with '&' or ','
#'
#' @return An object of class \link[=LAS-class]{LAS}
#'
#' @examples
#' LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
#' lidar = readLAS(LASfile)
#'
#' # Select the first returns classified as ground
#' firstground = filter_poi(lidar, Classification == 2L & ReturnNumber == 1L)
#'
#' # Multiple arguments are equivalent to &
#' firstground = filter_poi(lidar, Classification == 2L, ReturnNumber == 1L)
#'
#' # Multiple criteria
#' first_or_ground = filter_poi(lidar, Classification == 2L | ReturnNumber == 1L)
#' @name filters
#' @rdname filters
#' @md
NULL

#' @export
#' @rdname filters
filter_poi = function(las, ...)
{
  stopifnotlas(las)
  keep <- lasfilter_(las, lazyeval::dots_capture(...))

  # Memory optimization
  if (sum(keep) == nrow(las@data))
    return(las)

  return(las[keep])
}

lasfilter_ <- function(las, conditions)
{
  n <- nrow(las@data)
  combined_bools <- !logical(n)

  for (condition in conditions)
  {
    bools <- lazyeval::f_eval(condition, las@data)

    if (!is.logical(bools))
      stop("`conditions` must be logical.")

    bools[is.na(bools)] <- FALSE
    combined_bools <- combined_bools & bools
  }

  return(combined_bools)
}

parse_filter = function(las, filter, k)
{
  if (!is.null(filter))
    return(lasfilter_(las, list(filter)))

  return(TRUE)
}

#' @export
#' @rdname filters
filter_first = function(las)
{
  return(filter_nth(las, 1))
}

#' @export
#' @rdname filters
filter_firstlast = function(las)
{
  ReturnNumber <- NumberOfReturns <- NULL
  return(filter_poi(las, ReturnNumber == NumberOfReturns | ReturnNumber == 1))
}

#' @export
#' @rdname filters
filter_firstofmany = function(las)
{
  NumberOfReturns <- ReturnNumber <- NULL
  return(filter_poi(las, NumberOfReturns > 1, ReturnNumber == 1))
}

#' @export
#' @rdname filters
filter_ground = function(las)
{
  Classification <- NULL
  return(filter_poi(las, Classification == 2))
}

#' @export
#' @rdname filters
filter_last = function(las)
{
  NumberOfReturns <- ReturnNumber <- NULL
  return(filter_poi(las, ReturnNumber == NumberOfReturns))
}

#' @export
#' @rdname filters
filter_nth = function(las, n)
{
  ReturnNumber <- NULL
  return(filter_poi(las, ReturnNumber == n))
}

#' @export
#' @rdname filters
filter_single = function(las)
{
  NumberOfReturns <- NULL
  return(filter_poi(las, NumberOfReturns == 1))
}

#' @export
#' @rdname filters
filter_duplicates = function(las)
{
  UseMethod("filter_duplicates", las)
}

#' @export
#' @rdname filters
filter_duplicates.LAS = function(las)
{
  dup_xyz <- duplicated(las@data, by = c("X", "Y", "Z"))
  return(las[!dup_xyz])
}

#' @export
#' @rdname filters
filter_duplicates.LAScatalog = function(las)
{
  opt_select(las) <- "*"
  options <- list(need_buffer = FALSE, drop_null = TRUE, need_output_file = TRUE)
  output  <- catalog_map(las, filter_duplicates, .options = options)
  return(output)
}
