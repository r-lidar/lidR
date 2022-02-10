points = filter_poi(megaplot, Z > 2)
set.seed(42)
i = sample(1:npoints(points), 5000)
points = points[i]

n = 342

test_that("st_concave_hull works", {

  # Has been successfully tested on Solaris with rhub...
  # I'm tired to fix stuff on Solaris...
  skip_on_os("solaris")

  pts = points
  hull <- st_concave_hull(pts)
  expect_is(hull, "sfc")
  hull <- sf::st_coordinates(hull)
  expect_equal(dim(hull), c(n,4))
  expect_equivalent(hull[1,], hull[n,])

  hull <- st_concave_hull(pts, length_threshold = 10)
  hull <- sf::st_coordinates(hull)
  expect_equal(dim(hull), c(175,4))
  expect_equivalent(hull[1,], hull[175,])

  # It works with 3 points
  pts = points[1:3]
  hull <- st_concave_hull(pts)
  hull <- sf::st_coordinates(hull)
  expect_equal(dim(hull), c(4,4))
  expect_equivalent(hull[1,], hull[4,])

  # It does not work with < 3 points
  pts = points[1:2]
  expect_error(st_concave_hull(pts))

  # Large concavity result in convex hull
  hull <- st_concave_hull(points, concavity = 1e8)
  hull <- sf::st_coordinates(hull)
  expect_equal(dim(hull), c(29,4))
  expect_equivalent(hull[1,], hull[29,])

  skip_on_cran()

  set.seed(42)
  i = sample(1:npoints(points), 1000)
  points = points[i]
  hull <- st_concave_hull(points, method = "concavetin", max_length = 20)
  hull <- sf::st_coordinates(hull)
  expect_equal(dim(hull), c(75,4))
  expect_equivalent(hull[1,], hull[75,])
})

test_that("concaveman works", {

  # Has been successfully tested on Solaris with rhub...
  # I'm tired to fix stuff on Solaris...
  skip_on_os("solaris")

  # It works with matrix
  pts = as.matrix(sf::st_coordinates(points, z = FALSE))
  hull <- concaveman(pts)
  expect_equal(dim(hull), c(n,2))
  expect_equivalent(hull[1,], hull[n,])

  # It works with vectors
  x = points$X
  y = points$Y
  hull <- concaveman(x, y)
  expect_equal(dim(hull), c(n,2))
  expect_equivalent(hull[1,], hull[n,])
})
