% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{filters}
\alias{filters}
\alias{filter_poi}
\alias{filter_first}
\alias{filter_firstlast}
\alias{filter_firstofmany}
\alias{filter_ground}
\alias{filter_last}
\alias{filter_nth}
\alias{filter_single}
\alias{filter_duplicates}
\alias{filter_duplicates.LAS}
\alias{filter_duplicates.LAScatalog}
\alias{remove_noise}
\alias{remove_ground}
\alias{remove_water}
\title{Filter points of interest}
\usage{
filter_poi(las, ...)

filter_first(las)

filter_firstlast(las)

filter_firstofmany(las)

filter_ground(las)

filter_last(las)

filter_nth(las, n)

filter_single(las)

filter_duplicates(las)

\method{filter_duplicates}{LAS}(las)

\method{filter_duplicates}{LAScatalog}(las)

remove_noise(las)

remove_ground(las)

remove_water(las)
}
\arguments{
\item{las}{An object of class \link[=LAS-class]{LAS}}

\item{\dots}{Logical predicates. Multiple conditions are combined with '&' or ','}

\item{n}{integer  ReturnNumber == n}
}
\value{
An object of class \link[=LAS-class]{LAS}
}
\description{
Filter points of interest (POI) from a LAS object where conditions are true.
}
\details{
\itemize{
\item{\code{filter_poi} Select points of interest based on custom logical criteria.}
\item{\code{filter_first} Select only the first returns.}
\item{\code{filter_firstlast} Select only the first and last returns.}
\item{\code{filter_ground} Select only the returns classified as ground according to LAS specification.}
\item{\code{filter_last} Select only the last returns i.e. the last returns and the single returns.}
\item{\code{filter_nth} Select the returns from their position in the return sequence.}
\item{\code{filter_firstofmany} Select only the first returns from pulses which returned multiple points.}
\item{\code{filter_single} Select only the returns that return only one point.}
\item{\code{filter_duplicates} \strong{Removes} the duplicated points (duplicated by XYZ)}
\item{\code{remove_noise} Removes the returns classified as noise according to LAS specification.}
\item{\code{remove_ground} Removes the returns classified as ground according to LAS specification.}
\item{\code{remove_water} Removes the returns classified as water according to LAS specification.}
}
}
\section{Non-supported LAScatalog options}{

The option \code{select} is not supported and not respected because it always preserves the file format
and all the attributes. \code{select = "*"} is imposed internally.
}

\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
lidar = readLAS(LASfile)

# Select the first returns classified as ground
firstground = filter_poi(lidar, Classification == 2L & ReturnNumber == 1L)

# Multiple arguments are equivalent to &
firstground = filter_poi(lidar, Classification == 2L, ReturnNumber == 1L)

# Multiple criteria
first_or_ground = filter_poi(lidar, Classification == 2L | ReturnNumber == 1L)
}
