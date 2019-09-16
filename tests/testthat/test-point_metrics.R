context("point_metrics")

X = c(0,1,0,1,0,1)
Y = c(0,0,0.5,0.5,1,1)
Z = c(0,1,0.5,1.5,1,2)
D = data.table::data.table(X,Y,Z)
las = LAS(D)

test_that("points_metrics works with a single metric", {

  m = point_metrics(las, ~mean(Z), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 1))
  expect_equal(m$V1, c(0.5, 1, 0.5, 1.5, 1, 1.5))

  m = point_metrics(las, ~list(M = mean(Z)), k = 3L)

  expect_equal(names(m), "M")
})

test_that("points_metrics works with a multiple metrics", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 3))
  expect_equal(m$V3, c(0, 1, 0.5, 1.5, 1, 2))

  m = point_metrics(las, ~list(A = mean(Z), B = max(Z), C = Z[1]), k = 3L)

  expect_equal(names(m), c("A", "B", "C"))
})

test_that("points_metrics works with lidR metrics", {

  m = point_metrics(las, .stdmetrics_z, k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 36))
})

test_that("points_metrics works with nested function", {

  f <- function(x, y, z) {  return(max(c(x,y,z))) }
  g <- function(las) { point_metrics(las, ~f(X,Y,Z), k = 3L) }
  m <- g(las)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 1))
  expect_equal(m$V1, c(1, 1.5, 1, 2, 1.5, 2))
})

test_that("points_metrics fails nicely if error in func", {

  f <- function(x) {  stop("Dummy error") }

  expect_error(point_metrics(las, ~f(Z), k = 3L), "Dummy")
})
