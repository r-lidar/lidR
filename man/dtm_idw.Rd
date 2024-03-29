% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm-dtm.R
\name{dtm_idw}
\alias{dtm_idw}
\alias{knnidw}
\title{Spatial Interpolation Algorithm}
\usage{
knnidw(k = 10, p = 2, rmax = 50)
}
\arguments{
\item{k}{integer. Number of k-nearest neighbours. Default 10.}

\item{p}{numeric. Power for inverse-distance weighting. Default 2.}

\item{rmax}{numeric. Maximum radius where to search for knn. Default 50.}
}
\description{
This function is made to be used in \link{rasterize_terrain} or \link{normalize_height}. It implements an algorithm
for spatial interpolation. Interpolation is done using a k-nearest neighbour (KNN) approach with
an inverse-distance weighting (IDW).
}
\examples{
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)

#plot(las)

dtm = rasterize_terrain(las, algorithm = knnidw(k = 6L, p = 2))

#plot(dtm)
#plot_dtm3d(dtm)
}
\seealso{
Other dtm algorithms: 
\code{\link{dtm_kriging}},
\code{\link{dtm_tin}}
}
\concept{dtm algorithms}
