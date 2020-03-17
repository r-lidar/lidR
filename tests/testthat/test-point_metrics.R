context("point_metrics")

X = c(0,1,0,1,0,1)
Y = c(0,0,0.5,0.5,1,1)
Z = c(0,1,0.5,1.5,1,2)
I = 1:6
J = rep(TRUE, 6)
D = data.table::data.table(X,Y,Z,I,J)
las = LAS(D)

test_that("points_metrics works with a single metric (knn)", {

  m = point_metrics(las, ~mean(Z), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$V1, c(0.5, 1, 0.5, 1.5, 1, 1.5))
  expect_equal(names(m), c("pointID", "V1"))

  m = point_metrics(las, ~list(M = mean(Z)), k = 3L)

  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$M, c(0.5, 1, 0.5, 1.5, 1, 1.5))
  expect_equal(names(m), c("pointID", "M"))

  m = point_metrics(las, ~list(M = mean(Z)), k = 3L, xyz = TRUE)

  expect_equal(names(m), c("X", "Y", "Z", "M"))
})

test_that("points_metrics works with a single metric (shpere)", {

  m = point_metrics(las, ~length(Z), r = 0.8)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$V1, c(2,2,3,3,2,2))
  expect_equal(names(m), c("pointID", "V1"))

  m = point_metrics(las, ~list(M = mean(Z)), r = 0.8)

  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$M, c(0.25, 1.25, 0.5, 1.5, 0.75, 1.75))
  expect_equal(names(m), c("pointID", "M"))

  m = point_metrics(las, ~list(M = mean(Z)), r = 3L, xyz = TRUE)

  expect_equal(names(m), c("X", "Y", "Z", "M"))
})

test_that("points_metrics works with a single metric (knn + sphere)", {

  m1 = point_metrics(las, ~length(Z), k = 2, r = 0.8)
  m2 = point_metrics(las, ~length(Z), k = 3, r = 0.8)

  expect_equal(m1$V1, c(2,2,2,2,2,2))
  expect_equal(m2$V1, c(2,2,3,3,2,2))
})

test_that("points_metrics restpect the filter argument (knn)", {

  m = point_metrics(las, ~mean(Z), k = 3L, filter = ~I>2)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las)-2, 2))
  expect_equal(m$V1, c(1, 1.5, 1, 1.5))
})

test_that("points_metrics restpect the filter argument (shpere)", {

  m = point_metrics(las, ~mean(Z), r = 0.8, filter = ~I>2)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las)-2, 2))
  expect_equal(m$V1, c(0.75, 1.75, 0.75, 1.75))
})

test_that("points_metrics works with a multiple metrics (knn)", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), k = 3L)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 4))
  expect_equal(m$V3, c(0, 1, 0.5, 1.5, 1, 2))

  m = point_metrics(las, ~list(A = mean(Z), B = max(Z), C = Z[1]), k = 3L, xyz = TRUE)

  expect_equal(names(m), c("X", "Y", "Z", "A", "B", "C"))
})

test_that("points_metrics works with a multiple metrics (sphere)", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), r = 0.8)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 4))
  expect_equal(m$V1, c(0.25, 1.25, 0.5, 1.5, 0.75, 1.75))

  m = point_metrics(las, ~list(A = mean(Z), B = max(Z), C = Z[1]), r = 0.8, xyz = TRUE)

  expect_equal(names(m), c("X", "Y", "Z", "A", "B", "C"))
})


test_that("points_metrics works with lidR metrics", {

  m = point_metrics(las, .stdmetrics_z, k = 3L, xyz = FALSE)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 37))

  m = point_metrics(las, .stdmetrics_z, r = 0.8, xyz = FALSE)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 37))
})

test_that("points_metrics works with nested function", {

  f <- function(x, y, z) {  return(max(c(x,y,z))) }
  g <- function(las) { point_metrics(las, ~f(X,Y,Z), k = 3L, xyz = FALSE) }
  m <- g(las)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$V1, c(1, 1.5, 1, 2, 1.5, 2))

  f <- function(x, y, z) {  return(max(c(x,y,z))) }
  g <- function(las) { point_metrics(las, ~f(X,Y,Z), r = 0.8, xyz = FALSE) }
  m <- g(las)

  expect_is(m, "data.table")
  expect_equal(dim(m), c(npoints(las), 2))
  expect_equal(m$V1, c(0.5, 1.5, 1, 2, 1, 2))
})

test_that("points_metrics realloc memory", {

  m1 <- point_metrics(las, ~list(mean(Z), length(Z)), r = 2, alloc = 1)
  m2 <- point_metrics(las, ~list(mean(Z), length(Z)), r = 2)
  expect_equal(m1, m2)
})

test_that("points_metrics fails nicely if error in func", {

  f <- function(x) {  stop("Dummy error") }

  expect_error(point_metrics(las, ~f(Z), k = 3L), "Dummy")
  expect_error(point_metrics(las, ~f(Z), r = 3L), "Dummy")
})

test_that("points_metrics fails with non atomic output", {

  expect_error(point_metrics(las, ~c(1,2), k = 3L))
  expect_error(point_metrics(las, ~c(1,2), r = 0.8))
})

test_that("points_metrics fails", {

  expect_error(point_metrics(las, ~mean(Z)), "'k' or 'r' is missing")

  las@data$test = letters[1:6]
  expect_error(point_metrics(las, ~mean(Z), k = 3), "Incompatible type encountered")
})


test_that("points_metrics return references on coordinates", {

  m = point_metrics(las, ~list(mean(Z), max(Z), Z[1]), k = 3L, xyz = TRUE)

  expect_reference(m$X, las$X)
  expect_reference(m$Y, las$Y)
  expect_reference(m$Z, las$Z)
})

