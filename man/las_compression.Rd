% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/las_compression.R
\name{las_compression}
\alias{las_compression}
\alias{las_is_compressed}
\alias{las_size}
\title{Compression of the point cloud}
\usage{
las_is_compressed(las)

las_size(las)
}
\arguments{
\item{las}{A LAS object.}
}
\description{
Package rlas 1.6.0 supports compact representation of non populated attributes. For example \code{UserData}
is usually populated with zeros (not populated). Yet it takes 32 bits per point to store each 0.
With rlas 1.6.0 it can now use 644 bits no matter the number of points loaded if it is not
populated or populated with a unique value.
}
\details{
\code{las_is_compressed} test each attributes and returns a named vector with TRUE if the attribute is
compressed FALSE otherwise.\cr\cr
\code{las_size} returns the true size of a LAS object by considering the compression. \code{object.size} from
base R does not account for ALTREP and consequently cannot measure properly the size of a LAS object
}
\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)
las_is_compressed(las)

format(object.size(las), units = "MB")
format(las_size(las), units = "MB")

}
