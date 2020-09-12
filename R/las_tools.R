#' Rescale and reoffset a LAS object
#'
#' Modifies the scale factor and the offset of a LAS object. This function modifies the header and
#' recomputes the coordinates. Coordinates might be moved by few tenth of millimeters or few
#' millimeters depending of the accuracy imposed by the user.
#'
#' @param las An object of class LAS
#' @param xscale,yscale,zscale scalar. Can be missing if not relevant.
#' @param xoffset,yoffset,zoffset scalar. Can be missing if not relevant.
#'
#'
#' @export
#'
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package = "rlas")
#' las <- readLAS(LASfile)
#'
#' las <- las_rescale(las, xscale = 0.01, yscale = 0.01)
#' las <- las_reoffset(las, xoffset = 300000, yoffset = 5248000)
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

#' @rdname las_rescale
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
