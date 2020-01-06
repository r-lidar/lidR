context("triangulation")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, filter = "-keep_class 2 -keep_every_nth 10")

test_that("Boosted delaunay produces the correct output", {

  ps <- data.frame(
    X = c(0,  1, -1, 1, -1),
    Y = c(0, -1, 1, 1, -1))

  expected = structure(c(3L, 1L, 3L, 4L, 5L, 5L, 1L, 1L, 1L, 2L, 4L, 2L), .Dim = 4:3)

  ts <- lidR:::tDelaunay(ps, scales = c(1,1), offsets = c(0,0))

  expect_is(ts, "matrix")
  expect_equal(ts, expected)
})

test_that("Old delaunay produces the correct output", {

  ps <- data.frame(
    X = c(0,  1, -1, 1, -1),
    Y = c(0, -1, 1, 1, -1))
  ps <- as.matrix(ps)

  expected <- structure(c(1L, 1L, 5L, 5L, 4L, 4L, 1L, 1L, 3L, 2L, 3L, 2L), .Dim = 4:3)

  ts <- lidR:::tDelaunay(ps)

  expect_is(ts, "matrix")
  expect_equal(ts, expected)
})

test_that("Delaunay works with a LAS", {

  ts <- lidR:::tDelaunay(las)

  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(1608,3))
})

test_that("Boosted Delaunay trimming option works", {

  set.seed(42)
  X <- round(runif(50,0,10),2)
  set.seed(123)
  Y <- round(runif(50,0,10),2)
  ps <- data.frame(X,Y)

  ts <- lidR:::tDelaunay(ps, scale = c(0.01, 0.01), offsets = c(0,0), trim = 0)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(89,3))

  ts <- lidR:::tDelaunay(ps, scale = c(0.01, 0.01), offsets = c(0,0), trim = 2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(41,3))

  ts <- lidR:::tDelaunay(ps, scale = c(0.01, 0.01), offsets = c(0,0), trim = -2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(48,3))
})

test_that("Old Delaunay trimming option works", {

  set.seed(42)
  X <- round(runif(50,0,10),2)
  set.seed(123)
  Y <- round(runif(50,0,10),2)
  ps <- as.matrix(data.frame(X,Y))

  ts <- lidR:::tDelaunay(ps, trim = 0)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(89,3))

  ts <- lidR:::tDelaunay(ps, trim = 2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(41,3))

  ts <- lidR:::tDelaunay(ps, trim = -2)
  expect_is(ts, "matrix")
  expect_equal(dim(ts), c(48,3))
})

test_that("Delaunay fails when necessary", {

  ps <- data.frame(X = 1, Y = 2)
  expect_error(lidR:::tDelaunay(ps), "cannot triangulate less than 3 points")

  ps <- data.frame( A = c(0, 1, 1), B = c(0, 0, 1))
  expect_error(lidR:::tDelaunay(ps), "Columns are not named XY")

  ps <- 2
  expect_error(lidR:::tDelaunay(ps), "No method to triangulate this input")
})


test_that("Internal C_delaunay fails where necessary", {

  ps <- data.frame(
    X = c(0, -1),
    Y = c(0, -1))

  expect_error(lidR:::C_delaunay(ps, scales = c(1,1), offsets = c(0,0)), "cannot triangulate less than 3 points")

  ps <- data.frame(
    A = c(0, 1, 1),
    B = c(0, 0, 1))

  expect_error(lidR:::C_delaunay(ps, scales = c(1,1), offsets = c(0,0)), "columns are not named XY")

  ps <- data.frame(
    X = c(0, 1, 1),
    Y = c(0, 0, 1))

  expect_error(lidR:::C_delaunay(ps, scales = c(1,1), offsets = c(0.25,0)), "Scale factors are likely to be invalid")

  ps <- data.frame(
    X = runif(5, 0, 10),
    Y = runif(5, 0, 10))

  expect_error(lidR:::C_delaunay(ps, scales = c(1,1), offsets = c(0,0)), "Scale factors are likely to be invalid")
  expect_error(lidR:::C_delaunay(ps, scales = c(1,2), offsets = c(0,0)), "cannot triangulate points with different xy scale factors")
})

test_that("Internal C_Delaunay works with degenerated points", {

  ps <- data.frame(
    X = c(0, -1, 1, -1, 1, -1, 1, -1, 1, 0),
    Y = c(0, -1, -1, 1, 1, -1, -1, 1, 1, 0))

  expected = structure(c(4L, 1L, 4L, 5L, 2L, 2L, 1L, 1L, 1L, 3L, 5L, 3L), .Dim = 4:3)

  ts <- lidR:::C_delaunay(ps, scales = c(1,1), offsets = c(0,0))

  expect_is(ts, "matrix")
  expect_equal(ts, expected)
})

test_that("delaunay rasterization fall back to geometry", {

  ps <- data.frame(
    X = c(0,  1, -1, 1, -1),
    Y = c(0, -1, 1, 1, -1),
    Z = c(0, 1, -1, 0, 1))

  rs <- data.frame(
    X = c(0.2, 0.2),
    Y = c(0.8, -0.4))

  expect_message(lidR:::interpolate_delaunay(ps, rs, 0, scales = c(1,2), offsets = c(0,0)),  "reverted to the old slow method")
  ts <- suppressMessages(lidR:::interpolate_delaunay(ps, rs, 0, scales = c(1,2), offsets = c(0,0)))

  expect_equal(ts, c(-0.3, 0.4), tol = 1e-5)

  ps <- data.frame(
    X = c(0, 1, 1),
    Y = c(0, 0, 1))

  expect_message(lidR:::tDelaunay(ps, scales = c(1,1), offsets = c(0.25,0)), "reverted to the old slow method")
})
