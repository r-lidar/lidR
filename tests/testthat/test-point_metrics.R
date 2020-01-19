context("point_metrics")

X = c(0,1,0,1,0,1)
Y = c(0,0,0.5,0.5,1,1)
Z = c(0,1,0.5,1.5,1,2)
I = 1:6
J = rep(TRUE, 6)
D = data.table::data.table(X,Y,Z,I,J)
las = LAS(D)

test_that("points_metrics works with a single metric", {

  m = point_metrics(las, ~mean(Z), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 4))
  expect_equal(m$V1, c(0.5, 1, 0.5, 1.5, 1, 1.5))
  expect_equal(names(m), c("X", "Y", "Z", "V1"))

  m = point_metrics(las, ~list(M = mean(Z)), k = 3L)

  expect_equal(dim(m), c(npoints(las), 4))
  expect_equal(m$M, c(0.5, 1, 0.5, 1.5, 1, 1.5))
  expect_equal(names(m), c("X", "Y", "Z", "M"))

  m = point_metrics(las, ~list(M = mean(Z)), k = 3L, xyz = FALSE)

  expect_equal(names(m), c("M"))
})

test_that("points_metrics restpect the filter argument", {

  m = point_metrics(las, ~mean(Z), k = 3L, filter = ~I>2)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las)-2, 4))
  expect_equal(m$V1, c(1, 1.5, 1, 1.5))
})

test_that("points_metrics works with a multiple metrics", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 6))
  expect_equal(m$V3, c(0, 1, 0.5, 1.5, 1, 2))

  m = point_metrics(las, ~list(A = mean(Z), B = max(Z), C = Z[1]), k = 3L, xyz = FALSE)

  expect_equal(names(m), c("A", "B", "C"))
})

test_that("points_metrics works with lidR metrics", {

  m = point_metrics(las, .stdmetrics_z, k = 3L, xyz = FALSE)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 36))
})

test_that("points_metrics works with nested function", {

  f <- function(x, y, z) {  return(max(c(x,y,z))) }
  g <- function(las) { point_metrics(las, ~f(X,Y,Z), k = 3L, xyz = FALSE) }
  m <- g(las)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 1))
  expect_equal(m$V1, c(1, 1.5, 1, 2, 1.5, 2))
})

test_that("points_metrics fails nicely if error in func", {

  f <- function(x) {  stop("Dummy error") }

  expect_error(point_metrics(las, ~f(Z), k = 3L), "Dummy")
})

test_that("points_metrics fails with non atomic output", {

  expect_error(point_metrics(las, ~c(1,2), k = 3L))
})

test_that("points_metrics failst", {

  expect_error(point_metrics(las, ~mean(Z), r = 3), "Radius search is not supported yet")
  expect_error(point_metrics(las, ~mean(Z), k = 3, r = 3), "cannot be defined in the same time")
})


test_that("points_metrics return references on coordinates", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), k = 3L)

  expect_reference(m$X, las$X)
  expect_reference(m$Y, las$Y)
  expect_reference(m$Z, las$Z)
})

