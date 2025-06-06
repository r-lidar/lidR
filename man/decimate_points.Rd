% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decimate_points.R
\name{decimate_points}
\alias{decimate_points}
\title{Decimate a LAS object}
\usage{
decimate_points(las, algorithm)
}
\arguments{
\item{las}{An object of class \link[lidR:LAS-class]{LAS} or \link[lidR:LAScatalog-class]{LAScatalog}.}

\item{algorithm}{function. An algorithm of point decimation. \code{lidR} have: \link{random},
\link{homogenize}, \link{highest}, \link{lowest}, \link{random_per_voxel} and \link{barycenter_per_voxel}.}
}
\value{
If the input is a \code{LAS} object, returns a \code{LAS} object. If the input is a
\code{LAScatalog}, returns a \code{LAScatalog}.
}
\description{
Reduce the number of points using several possible algorithms.
}
\section{Non-supported LAScatalog options}{

The option `select` is not supported and not respected because it always preserves the file format
and all the attributes. `select = "*"` is imposed internally.\cr
The options `chunk buffer` is not supported and not respected because it is not needed.
}

\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, select = "xyz")

# Select points randomly to reach an overall density of 1
thinned1 <- decimate_points(las, random(1))
#plot(rasterize_density(las))
#plot(rasterize_density(thinned1))

# Select points randomly to reach an homogeneous density of 1
thinned2 <- decimate_points(las, homogenize(1,5))
#plot(rasterize_density(thinned2))

# Select the highest point within each pixel of an overlayed grid
thinned3 = decimate_points(las, highest(5))
#plot(thinned3)
}
