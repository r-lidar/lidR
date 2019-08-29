context("QuadTree")

X = c(0, 1, 1, 0, 0.5)
Y = c(0, 0, 1, 1, 0.5)
Z = c(0, 1, 1, 0, 0.5)

test_that("knn works", {
  x = 0.1
  y = 0.1

  nn = lidR:::C_knn(X,Y, x, y, 1, 1)

  expect_equal(nn$nn.idx, matrix(1))
  expect_equal(nn$nn.dist, matrix(sqrt(2)/10))

  nn = lidR:::C_knn(X,Y, x, y, 2, 1)

  expect_equal(nn$nn.idx, matrix(c(1,5), ncol = 2))
  expect_equal(nn$nn.dist, matrix(c(matrix(sqrt(2)/10), 4*sqrt(2)/10), ncol = 2))
})

test_that("knnidw works", {

  val1 = lidR:::C_knnidw(X,Y, Z, 0.1, 0.1, 1, 1, 1)
  val2 = lidR:::C_knnidw(X,Y, Z, 0.2, 0.2, 1, 1, 1)
  val3 = lidR:::C_knnidw(X,Y, Z, 0.3, 0.3, 1, 1, 1)
  val4 = lidR:::C_knnidw(X,Y, Z, 0.4, 0.5, 1, 1, 1)

  expect_equal(val1, 0)
  expect_equal(val2, 0)
  expect_equal(val3, 0.5)
  expect_equal(val4, 0.5)

  val1 = lidR:::C_knnidw(X,Y, Z, 0.25, 0.25, 2, 1, 1)
  val2 = lidR:::C_knnidw(X,Y, Z, 0.2, 0.2, 2, 1, 1)

  expect_equal(val1, 0.25)
  expect_equal(val2, 0.2)

  lidR:::C_knnidw(X,Y, Z, 0.25, 0.25, 2, 1, 1)
})

X = c(1, 2, 3, 10, 11, 12, 1, 2, 3)
Y = c(3, 2, 1, 5, 7, 6, 8, 9, 7)
Z = c(20, 1, 1, 5, 7, 6, 1, 1, 4)

test_that("QuandTree circle lookup works", {

  id = lidR:::C_circle_lookup(X,Y, 2,2,1)
  expect_equal(id, 2)

  id = lidR:::C_circle_lookup(X,Y, 1.9,3,1)
  expect_equal(id, 1)

  id = lidR:::C_circle_lookup(X,Y, 2,2,2)
  id = sort(id)
  expect_equal(id, 1:3)

  id = lidR:::C_circle_lookup(X,Y, 6,6,2)
  expect_equal(length(id), 0)
})


test_that("QuandTree knn 3d lookup works", {

  id = lidR:::C_knn3d_lookup(X,Y, Z,2,2,0,1)
  expect_equal(id, 2)

  id = lidR:::C_knn3d_lookup(X,Y,Z,2,2,2,4)
  id = sort(id)
  expect_equal(id, c(2,3,7,9))

  id = lidR:::C_knn3d_lookup(X,Y,Z,6,6,50,2)
  id = sort(id)
  expect_equal(id, c(1, 5))
})

X = c(0, 1, 1, 0, 0.5)
Y = c(0, 0, 1, 1, 0.5)
Z = c(0, 1, 1, 0, 0.5)

test_that("QuandTree knn works even with npoints < k", {

  nn = lidR:::C_knn(X,Y, c(0,2), c(1,2), 8, 1)

  expect <- matrix(c(4,5,1,3,2,0,0,0,3,5,2,4,1,0,0,0), ncol = 8, byrow = T)

  expect_equal(nn$nn.idx, expect)
})
