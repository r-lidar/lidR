context("utils_geometry")

x = c(0,1,1,0,0.5,0.2,0.3,0.4,0.2,0.8,0.9)
y = c(0,0,1,1,0.1,0.5,0.8,0.3,0.1,0.4,0.7)

vertx = c(0,1,0)
verty = c(0,0,1)

test_that("convex hull works", {
  expect_equal(lidR:::convex_hull(x,y), data.frame(x = c(1,0,0,1,1), y = c(0,0,1,1,0)))
})

test_that("area works", {
  expect_equal(lidR:::area(x,y), 1)
  expect_equal(lidR:::area(vertx,verty), 0.5)
})

test_that("points in polygon works", {
  ch = lidR:::convex_hull(x,y)

  expect_equal(lidR:::points_in_polygon(vertx, verty, x, y), c(T, F, F, F, T, T, F, T, T, F, F))
  expect_true(lidR:::point_in_polygon(ch$x, ch$y, 0.5,0.5))
  expect_true(!lidR:::point_in_polygon(ch$x, ch$y, -0.5,0.5))
})

test_that("tsearch works", {
  x <- c(-1, -1, 1)
  y <- c(-1, 1, -1)
  tri <- matrix(c(1, 2, 3), 1, 3)

  ## Should be in triangle #1
  ts <- lidR:::tsearch(x, y, tri, -1, -1)
  expect_equal(ts, 1)

  ## Should be in triangle #1
  ts <- lidR:::tsearch(x, y, tri, 1, -1)
  expect_equal(ts, 1)

  ## Should be in triangle #1
  ts <- lidR:::tsearch(x, y, tri, -1, 1)
  expect_equal(ts, 1)

  ## Centroid
  ts <- lidR:::tsearch(x, y, tri, -1/3, -1/3)
  expect_equal(ts, 1)

  ## Should be outside triangle #1, so should return NA
  ts <- lidR:::tsearch(x, y, tri, 1, 1)
  expect_true(is.na(ts))
})

test_that("tsearch pass computer precision tests", {
  x <- c(6.89, 7.15, 7.03)
  y <- c(7.76, 7.75, 8.35)
  tri <- matrix(c(1, 2, 3), 1, 3)

  ts <- lidR:::tsearch(x, y, tri, 7.125, 7.875)
  expect_equal(ts, 1)

  x <- c(278287.03, 278286.89, 278287.15)
  y <- c(602248.35, 602247.76, 602247.75)

  tri = matrix(c(1,2,3), 1,3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)

  tri = matrix(c(3,2,1), 1,3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)

  tri = matrix(c(2,3,1), 1,3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)

  tri = matrix(c(2,1,3), 1,3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)

  tri = matrix(c(3,1,2), 1,3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)

  tri <- matrix(c(1, 2, 3), 1, 3)
  ts <- lidR:::tsearch(x, y, tri, 278287.125, 602247.875)
  expect_equal(ts, 1)
})

test_that("tinfo works", {
  x <- c(0, 1, 0)
  y <- c(0, 0, 1)
  z <- c(1, 0, 0)
  X <- cbind(x, y, z)
  D <- matrix(c(1, 2, 3, 1), 1, 4)

  info = lidR:::tinfo(D, X)

  # normal vector is (1,1,1)
  n = c(info[,1], info[,2], info[,3]) %>% as.numeric
  expect_equal(n, c(1,1,1))

  # intercept is -1
  expect_equal(as.numeric(info[,4]), -1)

  # area is sqrt(3)/2
  expect_equal(as.numeric(info[,5]), sqrt(3)/2)

  # projected area is 0.5
  expect_equal(as.numeric(info[,6]), 0.5)

  # Max edge size is sqrt(2)
  expect_equal(as.numeric(info[,7]), sqrt(2))

  D <- matrix(c(1, 2, 3, 1), 1, 4)

  x = c(0,0,1)
  y = c(0,1,1)
  z = c(0,0,1)
  X = cbind(x,y,z)

  I = as.numeric(lidR:::tinfo(D, X))

  expect_equal(I[5], sqrt(2)/2)
  expect_equal(I[6], 1/2)

  x = c(0,0,1)
  y = c(0,1,0)
  z = c(0,1,1)
  X = cbind(x,y,z)

  I = as.numeric(lidR:::tinfo(D, X))

  expect_equal(I[5], sqrt(3)/2)
  expect_equal(I[6], 1/2)

  x = c(0,0,1)
  y = c(0,1,1)
  z = c(0,0,2)
  X = cbind(x,y,z)

  I = as.numeric(lidR:::tinfo(D, X))

  expect_equal(I[5], sqrt(5)/2)
  expect_equal(I[6], 1/2)
})


