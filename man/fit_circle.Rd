% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_shapes.R
\name{fit_circle}
\alias{fit_circle}
\title{Fits 2D Circles on a Point Cloud}
\usage{
fit_circle(points, num_iterations = 100, inlier_threshold = 0.01)
}
\arguments{
\item{points}{A LAS object representing a clustered slice, or an nx3 matrix of point coordinates.}

\item{num_iterations}{The number of iterations for the RANSAC fitting algorithm.}

\item{inlier_threshold}{A threshold value; points are considered inliers if their residuals are
below this value.}
}
\value{
A list containing the circle parameters and additional information for determining whether
the data fit a circular shape:
\itemize{
\item \code{center_x}, \code{center_y}, \code{radius}: parameters of the fitted circle.
\item \code{z}: average elevation of the circle.
\item \code{rmse}: root mean square error between the circle and all points.
\item \code{covered_arc_degree}: angular sector covered by inlier points.
\item \code{percentage_inlier}: percentage of points that are inliers
\item \code{percentage_inside}: percentage of points inside the circle
\item \code{inliers}: IDs of the inlier points.
}
}
\description{
Fits a 2D horizontally-aligned circle to a set of 3D points. The method uses RANSAC-based fitting
for robust estimation. The function returns a list with the circle parameters and additional data
to help determine whether the points form a circular shape. While it is always possible to fit a
circle to any dataset, the additional information assists in evaluating if the data genuinely
represent a circular pattern. The root mean square error (RMSE) may not always be a reliable metric
for assessing the quality of the fit due to non-circular elements (such as branches) that can arbitrarily
increase the RMSE of an otherwise well-fitted circle, as shown in the example below. Therefore, the
function also returns the angular range of the data, which indicates the spread of the inlier points:
360 degrees suggests a full circle, 180 degrees indicates a half-circle, or a smaller range may
suggest a partial arc.
}
\examples{
LASfile <- system.file("extdata", "dbh.laz", package="lidR")
las <- readTLS(LASfile, select = "xyz")
xyz = sf::st_coordinates(las)
circle = fit_circle(xyz)
plot(xyz, asp = 1, pch = 19, cex = 0.1)
symbols(circle$center_x, circle$center_y, circles = circle$radius,
  add = TRUE, fg = "red", inches = FALSE)

trunk = las[circle$inliers]

# Fitting a half-circle
half = xyz[xyz[,1] > 101.45,]
circle = fit_circle(half)
plot(half, asp = 1, pch = 19, cex = 0.1)
symbols(circle$center_x, circle$center_y, circles = circle$radius,
  add = TRUE, fg = "red", inches = FALSE)
circle$covered_arc_degree
}
