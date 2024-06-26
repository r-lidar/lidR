% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm-gnd.R
\name{gnd_csf}
\alias{gnd_csf}
\alias{csf}
\title{Ground Segmentation Algorithm}
\usage{
csf(
  sloop_smooth = FALSE,
  class_threshold = 0.5,
  cloth_resolution = 0.5,
  rigidness = 1L,
  iterations = 500L,
  time_step = 0.65
)
}
\arguments{
\item{sloop_smooth}{logical. When steep slopes exist, set this parameter to TRUE to reduce
errors during post-processing.}

\item{class_threshold}{scalar. The distance to the simulated cloth to classify a point cloud into ground
and non-ground. The default is 0.5.}

\item{cloth_resolution}{scalar. The distance between particles in the cloth. This is usually set to the
average distance of the points in the point cloud. The default value is 0.5.}

\item{rigidness}{integer. The rigidness of the cloth. 1 stands for very soft (to fit rugged
terrain), 2 stands for medium, and 3 stands for hard cloth (for flat terrain). The default is 1.}

\item{iterations}{integer. Maximum iterations for simulating cloth. The default value is 500. Usually,
there is no need to change this value.}

\item{time_step}{scalar. Time step when simulating the cloth under gravity. The default value
is 0.65. Usually, there is no need to change this value. It is suitable for most cases.}
}
\description{
This function is made to be used in \link{classify_ground}. It implements an algorithm for segmentation
of ground points base on a Cloth Simulation Filter. This method is a strict implementation of
the CSF algorithm made by Zhang et al. (2016) (see references) that relies on the authors' original
source code written and exposed to R via the the \code{RCSF} package.
}
\examples{
if (require(RCSF, quietly = TRUE))
{
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzrn")

mycsf <- csf(TRUE, 1, 1, time_step = 1)
las <- classify_ground(las, mycsf)
#plot(las, color = "Classification")
}
}
\references{
W. Zhang, J. Qi*, P. Wan, H. Wang, D. Xie, X. Wang, and G. Yan, “An Easy-to-Use Airborne LiDAR Data
Filtering Method Based on Cloth Simulation,” Remote Sens., vol. 8, no. 6, p. 501, 2016.
(http://www.mdpi.com/2072-4292/8/6/501/htm)
}
\seealso{
Other ground segmentation algorithms: 
\code{\link{gnd_mcc}},
\code{\link{gnd_pmf}}
}
\concept{ground segmentation algorithms}
