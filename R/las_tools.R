#' LAS utilities
#'
#' Tools to manipulate LAS objects maintaining compliance with
#' \href{https://www.asprs.org/wp-content/uploads/2019/07/LAS_1_4_r15.pdf}{ASPRS specification}
#'
#' In the specification of the LAS format the coordinates are expected to be given
#' with a certain precision e.g. 0.01 for a millimeter precision (or millifeet), meaning
#' that a file records e.g. 123.46 not 123.45678. Also, coordinates are stored as
#' integers. This is made possible with a scale and offset factor. For example,
#' 123.46 with an offset of 100 and a scale factor of 0.01 is actually stored as
#' (123.46 - 100)/0.01 = 2346. Storing 123.45678 with a scale factor of 0.01 and an offset
#' of 100 is invalid because it does not convert to an integer: (123.45678-100)/0.01
#' = 2345.678. Having an invalid LAS object may be critical in some lidR applications.
#' When writing into a LAS file, users will loose the extra precision without
#' warning and some algorithms in lidR use the integer conversion to make integer-based
#' computation and thus speed-up some algorithms and use less memory. Creation of an
#' invalid LAS object may cause problems and incorrect outputs.
#'
#' @param las An object of class LAS
#' @param x numeric. Coordinates vector
#' @param scale,offset scalar. scale and offset
#' @param xscale,yscale,zscale scalar. Can be missing if not relevant.
#' @param xoffset,yoffset,zoffset scalar. Can be missing if not relevant.
#' @param by_reference bool. Update the data in place without allocating new memory.
#' @param ... Unused.
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#'
#' # Manual modification of the coordinates (e.g. rotation, re-alignment, ...)
#' las@data$X <- las@data$X + 2/3
#' las@data$Y <- las@data$Y - 5/3
#'
#' # The point cloud is no longer valid
#' las_check(las)
#'
#' # It is important to fix that
#' las_quantize(las)
#'
#' # Now the file is almost valid
#' las_check(las)
#'
#' # Update the object to set up-to-date header data
#' las <- las_update(las)
#' las_check(las)
#'
#' # In practice the above code is not useful for regular users because the operators
#' # $<- already perform such operations on-the-fly. Thus the following
#' # syntax must be preferred and returns valid objects. Previous tools
#' # were only intended to be used in very specific cases.
#' las$X <- las$X + 2/3
#' las$Y <- las$Y - 5/3
#'
#' # Rescale and reoffset recompute the coordinates with
#' # new scales and offsets according to LAS specification
#' las <- las_rescale(las, xscale = 0.01, yscale = 0.01)
#' las <- las_reoffset(las, xoffset = 300000, yoffset = 5248000)
#' @rdname las_utilities
#' @name las_utilities
#' @family las utilities
NULL


#' @export
#' @rdname las_utilities
las_rescale = function(las, xscale, yscale, zscale)
{
  xoffset <- las@header@PHB[["X offset"]]
  yoffset <- las@header@PHB[["Y offset"]]
  zoffset <- las@header@PHB[["Z offset"]]

  if (!missing(xscale))
  {
    assert_is_a_number(xscale)
    newX <- round((las@data[["X"]] - xoffset)/xscale) * xscale + xoffset
    diff <- round(mean(abs(las@data[["X"]] - newX)), 4)
    las@data[["X"]] <- newX
    las@header@PHB[["X scale factor"]] <- xscale
    message(glue::glue("X coordinates were moved by {diff} on average"))
  }

  if (!missing(yscale))
  {
    assert_is_a_number(yscale)
    newY <- round((las@data[["Y"]] - yoffset)/yscale) * yscale + yoffset
    diff <- round(mean(abs(las@data[["Y"]] - newY)), 4)
    las@data[["Y"]] <- newY
    las@header@PHB[["Y scale factor"]] <- yscale
    message(glue::glue("Y coordinates were moved by {diff} on average"))
  }

  if (!missing(zscale))
  {
    assert_is_a_number(zscale)
    newZ <- round((las@data[["Z"]] - zoffset)/zscale) * zscale + zoffset
    diff <- round(mean(abs(las@data[["Z"]] - newZ)), 4)
    las@data[["Z"]] <- newZ
    las@header@PHB[["Z scale factor"]] <- zscale
    message(glue::glue("Z coordinates were moved by {diff} on average"))
  }

  las <- lasupdateheader(las)
  return(las)
}

#' @rdname las_utilities
#' @export
las_reoffset = function(las, xoffset, yoffset, zoffset)
{
  xscale  <- las@header@PHB[["X scale factor"]]
  yscale  <- las@header@PHB[["Y scale factor"]]
  zscale  <- las@header@PHB[["Z scale factor"]]

  xrange  <- c(las@header@PHB[["Min X"]], las@header@PHB[["Max X"]])
  yrange  <- c(las@header@PHB[["Min Y"]], las@header@PHB[["Max Y"]])
  zrange  <- c(las@header@PHB[["Min Z"]], las@header@PHB[["Max Z"]])

  if (!missing(xoffset))
  {
    assert_is_a_number(xoffset)

    newX <- suppressWarnings(as.integer(round((xrange - xoffset)/xscale)) * xscale + xoffset)
    if (anyNA(newX)) stop("Incorrect xoffset: integer overflow.", call. = FALSE)

    newX <- round((las@data[["X"]] - xoffset)/xscale) * xscale + xoffset
    diff <- round(mean(abs(las@data[["X"]] - newX)), 4)
    las@data[["X"]] <- newX
    las@header@PHB[["X offset"]] <- xoffset
    message(glue::glue("X coordinates were moved by {diff} on average"))
  }

  if (!missing(yoffset))
  {
    assert_is_a_number(yoffset)

    newY <- suppressWarnings(as.integer(round((yrange - yoffset)/yscale)) * yscale + yoffset)
    if (anyNA(newY)) stop("Incorrect yoffset: integer overflow.", call. = FALSE)

    newY <- round((las@data[["Y"]] - yoffset)/yscale) * yscale + yoffset
    diff <- round(mean(abs(las@data[["Y"]] - newY)), 4)
    las@data[["Y"]] <- newY
    las@header@PHB[["Y offset"]] <- yoffset
    message(glue::glue("Y coordinates were moved by {diff} on average"))
  }

  if (!missing(zoffset))
  {
    assert_is_a_number(zoffset)

    newZ <- suppressWarnings(as.integer(round((zrange - zoffset)/zscale)) * zscale + zoffset)
    if (anyNA(newZ)) stop("Incorrect zoffset: integer overflow.", call. = FALSE)

    newZ <- round((las@data[["Z"]] - zoffset)/zscale) * zscale + zoffset
    diff <- round(mean(abs(las@data[["Z"]] - newZ)), 4)
    las@data[["Z"]] <- newZ
    las@header@PHB[["Z offset"]] <- zoffset
    message(glue::glue("Z coordinates were moved by {diff} on average"))
  }

  las <- lasupdateheader(las)
  return(las)
}

#' @export
#' @rdname las_utilities
las_quantize = function(las, by_reference = TRUE)
{
  xscale <- las@header@PHB[["X scale factor"]]
  yscale <- las@header@PHB[["Y scale factor"]]
  zscale <- las@header@PHB[["Z scale factor"]]
  xoffset <- las@header@PHB[["X offset"]]
  yoffset <- las@header@PHB[["Y offset"]]
  zoffset <- las@header@PHB[["Z offset"]]

  if (isTRUE(by_reference))
  {
    quantize(las$X, xscale, xoffset)
    quantize(las$Y, yscale, yoffset)
    quantize(las$Z, zscale, zoffset)
    return(invisible(las))
  }
  else
  {
    las@data[["X"]] <- quantize(las$X, xscale, xoffset, FALSE)
    las@data[["Y"]] <- quantize(las$Y, yscale, yoffset, FALSE)
    las@data[["Z"]] <- quantize(las$Z, zscale, zoffset, FALSE)
    return(las)
  }
}

#' @export
#' @rdname las_utilities
las_update = function(las)
{
  stopifnotlas(las)

  header     <- as.list(las@header)
  new_header <- rlas::header_update(header, las@data)
  new_header <- LASheader(new_header)
  las@header <- new_header
  las@bbox[1,1] <- new_header@PHB[["Min X"]]
  las@bbox[1,2] <- new_header@PHB[["Max X"]]
  las@bbox[2,1] <- new_header@PHB[["Min Y"]]
  las@bbox[2,2] <- new_header@PHB[["Max Y"]]
  return(las)
}

#' @export
#' @rdname las_utilities
quantize = function(x, scale, offset, by_reference = TRUE, ...)
{
  umin = min(x)
  umax = max(x)
  urange = storable_coordinate_range(scale, offset)

  if (umax > urange[2] | umin < urange[1])
    stop("'x' contains unquantizable values out of the storable range.", call. = FALSE)

  if (isTRUE(by_reference))
  {
    fast_quantization(x, scale, offset)
    return(invisible(x))
  }
  else
  {
    y <- data.table::copy(x)
    fast_quantization(y, scale, offset)
    return(y)
  }
}

#' @export
#' @rdname las_utilities
is.quantized = function(x, scale, offset, ...)
{
  p <- list(...)
  if (!is.null(p$sample))
  {
    n <- min(100L, length(x))
    s <- as.integer(seq(1L, length(x), length.out = n))
    x <- x[s]
  }

  return(fast_countunquantized(x, scale, offset) == 0L)
}

#' @export
#' @rdname las_utilities
count_not_quantized = fast_countunquantized


#' @export
#' @rdname las_utilities
storable_coordinate_range <- function(scale, offset) {

  assert_is_a_number(scale)
  assert_is_a_number(offset)

  storable_min <- -2147483647 * scale + offset
  storable_max <-  2147483647 * scale + offset

  return(c("min" = storable_min, "max" = storable_max))
}


lasupdateheader = las_update
