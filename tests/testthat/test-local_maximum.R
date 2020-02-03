
X = runif(100, 0, 100)
Y = runif(100, 0, 100)
Z = rep(0, 100)
x = seq(5, 95, 5)
y = x
z = 1:length(x)
df = data.frame(X = c(X,x), Y = c(Y, y), Z = c(Z, z))
las = LAS(df)
epsg(las) <- 2008


test_that("local maximum works with disk", {

  lm = local_maximum(las, 14)

  expect_equal(length(lm), length(x))
  expect_equal(projection(lm), projection(las))
  expect_equal(names(lm), c("Z"))

  lm = local_maximum(las, 14.2)

  expect_equal(length(lm), 1)
})

test_that("local maximum works with rect", {

  lm = local_maximum(las, c(10, 8))

  expect_equal(length(lm), length(x))
  expect_equal(projection(lm), projection(las))

  lm = local_maximum(las, c(10, 10))

  expect_equal(length(lm), 1)
})

test_that("local maximum works with oriented rect", {

  lm = local_maximum(las, c(10, 10))

  expect_equal(length(lm), 1)

  lm = local_maximum(las, c(10, 10, pi/4))

  expect_equal(length(lm), length(x))

  lm = local_maximum(las, c(20, 9))

  expect_equal(length(lm), length(x))

  lm = local_maximum(las, c(20, 9, pi/4))

  expect_equal(length(lm), 1L)
})

test_that("local maximum returns an empty SpatialPoinstDataFrame", {

  X = runif(100, 0, 100)
  Y = runif(100, 0, 100)
  Z = rep(0, 100)
  I = sample(0:100, 100, TRUE)
  df = data.frame(X, Y, Z, Intensity = I)
  las = LAS(df)

  lm = local_maximum(las, 10)

  expect_is(lm, "SpatialPointsDataFrame")
  expect_equal(length(lm), 0)
  expect_equal(names(lm), c("Z", "Intensity"))
})

test_that("local maximum throw errors", {

  expect_error(local_maximum(las, c(10, 20, 45)), "not all in range")
  expect_error(local_maximum(las, -5), "not all positive")
})
