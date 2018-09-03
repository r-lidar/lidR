% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LASmethods-generic.r, R/LASmethods-LAS.r,
%   R/LASmethods-LAScatalog.r
\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,LAS,missing-method}
\alias{plot,LAScatalog,missing-method}
\title{Plot a LAS* object}
\usage{
plot(x, y, ...)

\S4method{plot}{LAS,missing}(x, y, color = "Z",
  colorPalette = height.colors(50), bg = "black", trim = 1,
  backend = c("rgl", "pcv"), ...)

\S4method{plot}{LAScatalog,missing}(x, y, mapview = TRUE, ...)
}
\arguments{
\item{x}{A \code{LAS*} object}

\item{y}{Unused (inherited from R base)}

\item{...}{Will be passed to \link[rgl:points3d]{points3d} (LAS) or \link[graphics:plot]{plot}
if \code{mapview = FALSE} or to \link[mapview:mapView]{mapview} if \code{mapview = TRUE} (LAScatalog).}

\item{color}{characters. The field used to color the points. Default is Z coordinates.}

\item{colorPalette}{characters. A list of colors such as that generated by heat.colors,
topo.colors, terrain.colors or similar functions.  Default is \code{height.colors(50)}
provided by the package \code{lidR}}

\item{bg}{The color for the background. Default is black.}

\item{trim}{numeric. Enables trimming of values when outliers break the color palette range.
Default is 1, meaning that the whole range of values is used for the color palette.
0.9 means that 10\% of the highest values are not used to define the color palette.
In this case values higher than the 90th percentile are set to the highest color.
They are not removed.}

\item{backend}{character. Can be \code{"rgl"} or \code{"pcv"}. If \code{"rgl"} is chosen
the display relies on the \code{rgl} package. If \code{"pcv"} is chosen it relies on the
\code{PointCloudViewer} package which is much more efficient and can handle million of points
using few memory. \code{PointCloudViewer} is not available on CRAN yet and should
be install from github (see. \url{https://github.com/Jean-Romain/PointCloudViewer}).}

\item{mapview}{logical. If \code{FALSE} the catalog is displayed in a regular plot from R base.}
}
\description{
Plot displays a 3D interactive windows based on rgl for \link{LAS} objects

Plot displays an interactive view for \link[lidR:LAScatalog-class]{LAScatalog} objects with pan and
zoom capabilities based on \link[mapview:mapview-package]{mapview}. If the coordinate reference
system (CRS) of the \code{LAScatalog} is non empty, the plot can be displayed on top of base maps
(satellite data, elevation, street, and so on).
}
\section{Methods (by class)}{
\itemize{
\item \code{x = LAScatalog,y = missing}: plot LAScatalog
}}

\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

plot(las)

# Outliers of intensity breaks the color range. Use the trim parameter.
plot(las, color = "Intensity", colorPalette = heat.colors(50))
plot(las, color = "Intensity", colorPalette = heat.colors(50), trim = 0.99)

\dontrun{

# single file catalog using data provided in lidR
ctg = catalog(LASfile)
plot(ctg)
}
}