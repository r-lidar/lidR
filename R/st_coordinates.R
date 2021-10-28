#' Coordinates of LAS* object in a matrix form
#'
#' Retrieve coordinates of a LAS* object in matrix form. It creates a copy of the coordinates
#'
#' @template param-las
#' @param ... unused.
#'
#' @return matrix
#'
#' @name st_coordinates
#' @md
#' @noRd
st_coordinates <- function(x, ...)
{
  UseMethod("st_coordinates", x)
}

st_coordinates.LAS <- function(x, ...)
{
  return(as.matrix(coordinates3D(x)))
}

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
