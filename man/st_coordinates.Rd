% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_coordinates.R
\name{st_coordinates}
\alias{st_coordinates}
\alias{st_coordinates.LAS}
\alias{st_coordinates.LAScatalog}
\title{Coordinates of a LAS* object in a matrix form}
\usage{
\method{st_coordinates}{LAS}(x, z = TRUE, ...)

\method{st_coordinates}{LAScatalog}(x, ...)
}
\arguments{
\item{x}{A LAS* object}

\item{z}{bool. Return XY or XYZ matrix}

\item{...}{unused.}
}
\value{
matrix
}
\description{
Retrieve coordinates of a \verb{LAS*} object in matrix form. It creates a copy of the coordinates
because of the coercion from \code{data.frame} to \code{matrix}. This function inherits \code{sf::st_coordinates}
}
\examples{
LASfile <- system.file("extdata", "example.laz", package="rlas")
las <- readLAS(LASfile)
sf::st_coordinates(las)
}
