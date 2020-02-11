
X = runif(100, 0, 100)
Y = runif(100, 0, 100)
Z = rep(0, 100)
x = seq(5, 95, 5)
y = x
z = 1:length(x)
df = data.frame(X = c(X,x), Y = c(Y, y), Z = c(Z, z))
las = LAS(df)
epsg(las) <- 2008


test_that("find_localmaxima works with disk", {

  lm = find_localmaxima(las, 14)

  expect_equal(length(lm), length(x))
  expect_equal(projection(lm), projection(las))
  expect_equal(names(lm), c("Z"))

  lm = find_localmaxima(las, 14.2)

  expect_equal(length(lm), 1)
})

test_that("find_localmaxima works with rect", {

  lm = find_localmaxima(las, c(10, 8))

  expect_equal(length(lm), length(x))
  expect_equal(projection(lm), projection(las))

  lm = find_localmaxima(las, c(10, 10))

  expect_equal(length(lm), 1)
})

test_that("find_localmaxima works with oriented rect", {

  lm = find_localmaxima(las, c(10, 10))

  expect_equal(length(lm), 1)

  lm = find_localmaxima(las, c(10, 10, pi/4))

  expect_equal(length(lm), length(x))

  lm = find_localmaxima(las, c(20, 9))

  expect_equal(length(lm), length(x))

  lm = find_localmaxima(las, c(20, 9, pi/4))

  expect_equal(length(lm), 1L)
})

test_that("find_localmaxima returns an empty SpatialPoinstDataFrame", {

  X = runif(100, 0, 100)
  Y = runif(100, 0, 100)
  Z = rep(0, 100)
  I = sample(0:100, 100, TRUE)
  df = data.frame(X, Y, Z, Intensity = I)
  las = LAS(df)

  lm = find_localmaxima(las, 10)

  expect_is(lm, "SpatialPointsDataFrame")
  expect_equal(length(lm), 0)
  expect_equal(names(lm), c("Z", "Intensity"))
})

test_that("find_localmaxima throw errors", {

  expect_error(find_localmaxima(las, c(10, 20, 45)), "not all in range")
  expect_error(find_localmaxima(las, -5), "not all positive")
})
