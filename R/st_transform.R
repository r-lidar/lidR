#' Transform or convert coordinates of LAS objects
#'
#' Transform or convert coordinates of LAS objects  `st_transform()` extends `sf::st_transform()`

#'
#' @param x, An object of class LAS
#' @param crs crs object from sf or CRS object from sp
#' @param ... additional arguments `scale` and `xoffset`, `yoffset`, `zoffset`
#' for the output LAS objects. It is not  mandatory but recommended to consider providing such
#' information. Otherwise it will be guessed automatically which might not be the best choice.
#'
#' @return A LAS object
#'
#' @name st_transform
#' @importFrom sf st_transform
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las = readLAS(LASfile)
#' st_crs(las)$Name
#' st_bbox(las)
#' tlas <- sf::st_transform(las, sf::st_crs(26918))
#' st_crs(tlas)$Name
#' st_bbox(tlas)
#' @md
#' @export
#' @rdname st_transform
st_transform.LAS <- function(x, crs, ...)
{
  if (is.na(st_crs(x)))
    stop("No transformation possible from NA reference system")

  p <- list(...)

  # Transform the point coordinates
  sfpts <- sf::st_multipoint(as.matrix(coordinates3D(x)))
  sfpts <- sf::st_geometry(sfpts)
  sfpts <- sf::st_set_crs(sfpts, st_crs(x))
  sfpts <- sf::st_transform(sfpts, crs)
  geom  <- sf::st_geometry(sfpts)
  X <- geom[[1]][,1]
  Y <- geom[[1]][,2]
  Z <- geom[[1]][,3]

  # Update the offsets in the header
  offsetx <- floor(min(X))
  offsety <- floor(min(Y))
  offsetz <- floor(min(Z))
  if (!is.null(p$xoffset)) offsetx <- p$xoffset
  if (!is.null(p$yoffset)) offsety <- p$yoffset
  if (!is.null(p$zoffset)) offsetz <- p$zoffset
  x@header@PHB[["X offset"]] <- offsetx
  x@header@PHB[["Y offset"]] <- offsety
  x@header@PHB[["Z offset"]] <- offsetz

  # Update the scale factors
  scalex <- x[["X scale factor"]]
  scaley <- x[["Y scale factor"]]
  scalez <- x[["Z scale factor"]]
  if (!is.null(p$scale)) scalex <- scaley <- scalez <- p$scale
  x@header@PHB[["X scale factor"]] <- scalex
  x@header@PHB[["Y scale factor"]] <- scaley
  x@header@PHB[["Z scale factor"]] <- scalez

  # Quantize the coordinates
  fast_quantization(X, scalex, offsetx)
  fast_quantization(Y, scaley, offsety)
  fast_quantization(Z, scalez, offsetz)

  # Update the LAS object
  x@data[["X"]] <- X
  x@data[["Y"]] <- Y
  x@data[["Z"]] <- Z

  x <- las_update(x)
  st_crs(x) <- crs

  return(x)
}
