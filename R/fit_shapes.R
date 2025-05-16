fit_circle_on_3_points <- function(points_subset)
{
  stopifnot(nrow(points_subset) == 3L, ncol(points_subset) > 2L)

  # Extract the coordinates
  x1 <- points_subset[1, 1]
  y1 <- points_subset[1, 2]
  x2 <- points_subset[2, 1]
  y2 <- points_subset[2, 2]
  x3 <- points_subset[3, 1]
  y3 <- points_subset[3, 2]

  # Calculate the coefficients for the linear system
  A <- 2 * (x2 - x1)
  B <- 2 * (y2 - y1)
  C <- x2^2 + y2^2 - x1^2 - y1^2
  D <- 2 * (x3 - x1)
  E <- 2 * (y3 - y1)
  G <- x3^2 + y3^2 - x1^2 - y1^2

  # Solve for a and b using Cramer's rule
  denominator <- A * E - B * D
  if (denominator == 0)
  {
    return(c(0, 0, 0))
  }
  a <- (C * E - B * G) / denominator
  b <- (A * G - C * D) / denominator

  # Calculate the radius
  r <- sqrt((x1 - a)^2 + (y1 - b)^2)

  # Return the center and radius
  return(c(a, b, r))
}

# fit_circle_on_3_points <- function(points_subset)
# {
#   stopifnot(nrow(points_subset) == 3L, ncol(points_subset) > 2L)
#
#   # Initial guesses
#   x_mean <- mean(points_subset[, 1])
#   y_mean <- mean(points_subset[, 2])
#   r_guess <- sqrt(mean((points_subset[, 1] - x_mean)^2 + (points_subset[, 2] - y_mean)^2))
#   initial_guess <- c(x_mean, y_mean, r_guess)
#
#   # Optimize circle parameters
#   fit <- optim(par = initial_guess, fn = residuals_circle, points = points_subset)
#   return(fit$par)
# }

#' Fits 2D Circles on a Point Cloud
#'
#' Fits a 2D horizontally-aligned circle to a set of 3D points. The method uses RANSAC-based fitting
#' for robust estimation. The function returns a list with the circle parameters and additional data
#' to help determine whether the points form a circular shape. While it is always possible to fit a
#' circle to any dataset, the additional information assists in evaluating if the data genuinely
#' represent a circular pattern. The root mean square error (RMSE) may not always be a reliable metric
#' for assessing the quality of the fit due to non-circular elements (such as branches) that can arbitrarily
#' increase the RMSE of an otherwise well-fitted circle, as shown in the example below. Therefore, the
#' function also returns the angular range of the data, which indicates the spread of the inlier points:
#' 360 degrees suggests a full circle, 180 degrees indicates a half-circle, or a smaller range may
#' suggest a partial arc.
#'
#' @param points A LAS object representing a clustered slice, or an nx3 matrix of point coordinates.
#' @param num_iterations The number of iterations for the RANSAC fitting algorithm.
#' @param inlier_threshold A threshold value; points are considered inliers if their residuals are
#' below this value.
#'
#' @return A list containing the circle parameters and additional information for determining whether
#' the data fit a circular shape:
#' - `center_x`, `center_y`, `radius`: parameters of the fitted circle.
#' - `z`: average elevation of the circle.
#' - `rmse`: root mean square error between the circle and all points.
#' - `covered_arc_degree`: angular sector covered by inlier points.
#' - `percentage_inlier`: percentage of points that are inliers
#' - `percentage_inside`: percentage of points inside the circle
#' - `inliers`: IDs of the inlier points.
#' @md
#' @export
#' @examples
#' LASfile <- system.file("extdata", "dbh.laz", package="lidR")
#' las <- readTLS(LASfile, select = "xyz")
#' xyz = sf::st_coordinates(las)
#' circle = fit_circle(xyz)
#' plot(xyz, asp = 1, pch = 19, cex = 0.1)
#' symbols(circle$center_x, circle$center_y, circles = circle$radius,
#'   add = TRUE, fg = "red", inches = FALSE)
#'
#' trunk = las[circle$inliers]
#'
#' # Fitting a half-circle
#' half = xyz[xyz[,1] > 101.45,]
#' circle = fit_circle(half)
#' plot(half, asp = 1, pch = 19, cex = 0.1)
#' symbols(circle$center_x, circle$center_y, circles = circle$radius,
#'   add = TRUE, fg = "red", inches = FALSE)
#' circle$covered_arc_degree
fit_circle <- function(points, num_iterations = 100, inlier_threshold = 0.01)
{
  best_circle <- NULL
  max_inliers <- 0

  if (is(points, "LAS")) points = sf::st_coordinates(points)

  stopifnot(is.matrix(points), ncol(points) == 3L, nrow(points) > 3L)

  z = points[, 3]

  for (i in 1:num_iterations)
  {
    # Randomly sample points
    sample_indices <- sample(1:nrow(points), 3L)
    points_subset <- points[sample_indices, ]

    params <- fit_circle_on_3_points(points_subset)

    # Compute residuals for all points
    distances <- sqrt((points[, 1] - params[1])^2 + (points[, 2] - params[2])^2)
    residuals <- abs(distances - params[3])

    # Count inliers (points whose residuals are below the threshold)
    inliers <- sum(residuals < inlier_threshold)

    # Update best model if more inliers are found
    if (inliers > max_inliers)
    {
      max_inliers <- inliers
      best_circle <- params
    }
  }

  if (is.null(best_circle))
  {
    center_x = mean(points[,1])
    center_y = mean(points[,2])
    radius = 0.001
    z = mean(z)
    angle_range_degrees = 0
    inlier_ratio = 0
    percentage_inside = 0
    inliers = rep(TRUE, nrow(points))
    cfqi = 0
  }
  else
  {
    center_x <- best_circle[1]
    center_y <- best_circle[2]
    radius   <- best_circle[3]

    # Goodness of fit
    distances <- sqrt((points[, 1] - center_x)^2 + (points[, 2] - center_y)^2)
    residuals <- abs(distances - radius)
    inliers <- residuals < inlier_threshold

    # Angular range
    angle_res = 3
    angles <- atan2(points[inliers, 2] - center_y, points[inliers, 1] - center_x)
    angles <- ifelse(angles < 0, angles + 2 * pi, angles)
    angles <- sort(angles*180/pi)
    rangles <- unique(round(angles/angle_res)*angle_res)

    angle_range_degrees = sum(diff(rangles) <= angle_res)*angle_res
    arc_score <- angle_range_degrees / 360

    # Inlier ratio
    inlier_ratio <- sum(inliers) / nrow(points)

    # Percentage of point inside the circle
    ninside = sum(distances < radius - inlier_threshold)
    percentage_inside = ninside/nrow(points)
    score_inside = 1-percentage_inside
  }

  return(list(center_x = center_x,
              center_y = center_y,
              radius = radius,
              z = mean(z),
              covered_arc_degree = angle_range_degrees,
              percentage_inlier = inlier_ratio,
              percentage_inside = percentage_inside,
              inliers = which(inliers)))
}
