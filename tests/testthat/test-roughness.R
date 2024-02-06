context("roughness")

skip_if_not_installed("geometry")

test_that("Flat surfaces have a rumple index of 1. Delaunay method", {
  n = sample(10:20, 1)
  x = runif(n, 0, 100)
  y = runif(n, 0, 100)
  z = rep(10, n)

  expect_equal(rumple_index(x, y, z), 1)
  expect_equal(rumple_index(x/10, y/5, z), 1)
  expect_equal(rumple_index(x, y, z*2), 1)
})

test_that("Flat surfaces have a rumple index of 1. Jenness method", {
  skip_if_not_installed("sp")
  bbox = terra::ext(0,10,0,10)
  r = terra::rast(bbox, res = 0.5)
  r[] = 2.56

  expect_equal(rumple_index(r), 1)
})

test_that("Rought surfaces have the good rumple index. Delaunay", {
  n = sample(10:20, 1)
  x = runif(n, 0, 100)
  y = runif(n, 0, 100)
  z = x

  expect_equal(rumple_index(x, y, z), sqrt(2))

  z = x + y

  expect_equal(rumple_index(x, y, z), sqrt(3))
})

test_that("Error handling. Delaunay", {

  x = runif(3, 0, 100)
  y = runif(3, 0, 100)
  z = x

  expect_equal(rumple_index(x, y, z), NA_real_)

  # x = runif(4, 0, 100)
  # y = x
  # z = x
  #
  # expect_equal(suppressMessages(rumple_index(x, y, z)), NA_real_)
})
