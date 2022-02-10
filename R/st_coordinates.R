#' Coordinates of a LAS* object in a matrix form
#'
#' Retrieve coordinates of a `LAS*` object in matrix form. It creates a copy of the coordinates
#' because of the coercion from `data.frame` to `matrix`. This function inherits `sf::st_coordinates`
#'
#' @param x A LAS* object
#' @param z bool. Return XY or XYZ matrix
#' @param ... unused.
#'
#' @return matrix
#'
#' @name st_coordinates
#' @rdname st_coordinates
#' @importFrom sf st_coordinates
#' @md
#' @examples
#' LASfile <- system.file("extdata", "example.laz", package="rlas")
#' las <- readLAS(LASfile)
#' sf::st_coordinates(las)
NULL

#' @rdname st_coordinates
#' @export
st_coordinates.LAS <- function(x, z = TRUE, ...)
{
  if (isTRUE(z))
    return(as.matrix(coordinates3D(x)))
  else
    return(as.matrix(coordinates(x)))
}

#' @rdname st_coordinates
#' @export
st_coordinates.LAScatalog <- function(x, ...)
{
  return(sf::st_coordinates(x@data))
}

coordinates = function(las)
{
  X  <- las[["X"]]
  Y  <- las[["Y"]]
  DF <- data.frame(X,Y)
  data.table::setDT(DF)
  return(DF)
}

coordinates3D = function(las)
{
  X  <- las[["X"]]
  Y  <- las[["Y"]]
  Z  <- las[["Z"]]
  DF <- data.frame(X,Y,Z)
  data.table::setDT(DF)
  return(DF)
}
