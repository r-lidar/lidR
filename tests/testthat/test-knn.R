context("knn")

X = c(0, 1, 1, 0, 0.5)
Y = c(0, 0, 1, 1, 0.5)
Z = c(0, 1, 1, 0, 0.5)

test_that("knn works", {
  x = 0.1
  y = 0.1

  nn = lidR:::C_knn(X,Y, x, y, 1)

  expect_equal(nn$nn.idx, matrix(1))
  expect_equal(nn$nn.dist, matrix(sqrt(2)/10))

  nn = lidR:::C_knn(X,Y, x, y, 2)

  expect_equal(nn$nn.idx, matrix(c(1,5), ncol = 2))
  expect_equal(nn$nn.dist, matrix(c(matrix(sqrt(2)/10), 4*sqrt(2)/10), ncol = 2))
})

test_that("knnidw works", {

  val1 = lidR:::C_knnidw(X,Y, Z, 0.1, 0.1, 1, 1)
  val2 = lidR:::C_knnidw(X,Y, Z, 0.2, 0.2, 1, 1)
  val3 = lidR:::C_knnidw(X,Y, Z, 0.3, 0.3, 1, 1)
  val4 = lidR:::C_knnidw(X,Y, Z, 0.4, 0.5, 1, 1)

  expect_equal(val1, 0)
  expect_equal(val2, 0)
  expect_equal(val3, 0.5)
  expect_equal(val4, 0.5)

  val1 = lidR:::C_knnidw(X,Y, Z, 0.25, 0.25, 2, 1)
  val2 = lidR:::C_knnidw(X,Y, Z, 0.2, 0.2, 2, 1)

  expect_equal(val1, 0.25)
  expect_equal(val2, 0.2)

  lidR:::C_knnidw(X,Y, Z, 0.25, 0.25, 2, 1)
})
