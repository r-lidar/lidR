#' Tools inherited from base R for LAS* objects
#'
#' Tools inherited from base R for LAS* objects
#'
#' @param x a LAS* object
#' @param ... LAS* objects if it is the sole argurment (e.g. in rbind())
#'
#' @name tools
NULL

#' @export
#' @rdname tools
dim.LAS <- function(x) return(dim(x@data))

#' @export
#' @rdname tools
dim.LAScatalog <- function(x) return(dim(x@data))

#' @export
#' @rdname tools
ncol.LAS <- function(x) return(ncol(x@data))

#' @export
#' @rdname tools
nrow.LAScatalog <- function(x) return(nrow(x@data))

#' @export
#' @rdname tools
names.LAS <- function(x) return(names(x@data))

#' @export
names.LAScatalog <- function(x) return(names(x@data))

#' @name tools
#' @export
rbind.LAS <- function(...)
{
  dots <- list(...)
  names(dots) <- NULL
  assert_all_are_same_crs(dots)

  xscales <- sapply(dots, function(x) x@header@PHB[["X scale factor"]])
  yscales <- sapply(dots, function(x) x@header@PHB[["Y scale factor"]])
  zscales <- sapply(dots, function(x) x@header@PHB[["Z scale factor"]])
  xoffsets <- sapply(dots, function(x) x@header@PHB[["X offset"]])
  yoffsets <- sapply(dots, function(x) x@header@PHB[["Y offset"]])
  zoffsets <- sapply(dots, function(x) x@header@PHB[["Z offset"]])

  need_quantization <- FALSE
  if (length(unique(xscales)) != 1L || length(unique(yscales)) != 1L || length(unique(zscales)) != 1L ||
      length(unique(xoffsets)) != 1L || length(unique(yoffsets)) != 1L || length(unique(zoffsets)) != 1L)
  {
    warning("Different LAS objects have different scales and/or offsets. The first object was used as reference to quantize the others.", call. = FALSE)
    need_quantization <- TRUE
  }

  data <- data.table::rbindlist(lapply(dots, function(x) { x@data } ))

  if (need_quantization)
  {
    quantize(data[["X"]], xscales[1], xoffsets[1])
    quantize(data[["Y"]], yscales[1], yoffsets[1])
    quantize(data[["Z"]], zscales[1], zoffsets[1])
  }

  return(LAS(data, dots[[1]]@header, st_crs(dots[[1]]), index = dots[[1]]@index))
}

#' @name rbind
#' @export
rbind.LAScatalog <- function(...)
{
  dots <- list(...)
  names(dots) <- NULL
  assert_all_are_same_crs(dots)

  data <- do.call(rbind, lapply(dots, function(x) { x@data } ))
  ctg <- dots[[1]]
  ctg@data <- data

  chk <- las_check(ctg, print = FALSE)
  for (msg in chk$warnings) warning(msg, call. = FALSE)
  for (msg in chk$errors) warning(msg, call. = FALSE)

  return(ctg)
}

#' @export
#' @rdname tools
npoints <- function(x, ...) { UseMethod("npoints", x) }

#' @export
npoints.LAS <- function(x, ...) { return(nrow(x@data)) }

#' @export
npoints.LASheader <- function(x, ...) { return(x@PHB[["Number of point records"]]) }

#' @export
npoints.LAScatalog <- function(x, ...) { return(sum(x[["Number.of.point.records"]])) }

#' @rdname tools
#' @export
density <- function(x, ...) { UseMethod("density", x) }

#' @export
#' @importFrom stats density
density.LAS <- function(x, ...) { return(npoints(x)/area(x)) }

#' @export
density.LASheader <- function(x, ...) { return(npoints(x)/area(x)) }

#' @export
density.LAScatalog <- function(x, ...) { return(npoints(x)/area(x)) }
