context("roughness")

test_that("Flat surfaces have a rumple index of 1", {
  n = sample(20:50, 1)
  x = runif(n, 0, 100)
  y = runif(n, 0, 100)
  z = rep(10, n)

  expect_equal(rumple_index(x, y, z), 1)
  expect_equal(rumple_index(x/10, y/5, z), 1)
  expect_equal(rumple_index(x, y, z*2), 1)
})

test_that("Rought surfaces have a rumple index > 1", {
  n = sample(20:50, 1)
  x = runif(n, 0, 100)
  y = runif(n, 0, 100)
  z = runif(n, 0, 10)

  expect_gt(rumple_index(x, y, z), 1)
  expect_gt(rumple_index(x/10, y/5, z), 1)
  expect_gt(rumple_index(x, y, z*2), 1)
})

test_that("The Roughter the surface the greater the rumple index", {
  n = sample(20:50, 1)
  x = runif(n, 0, 100)
  y = runif(n, 0, 100)
  z1 = runif(n, 0, 10)
  z3 = runif(n, 0, 30)

  ri1 = rumple_index(x, y, z1)
  ri3 = rumple_index(x, y, z3)
  ri4 = rumple_index(x/2, y/2, z1)

  expect_gt(ri3, ri1)
  expect_gt(ri4, ri1)
})


test_that("rumple_index support CHM", {
  las = lidR:::dummy_las(3000)
  chm = las %>% grid_canopy(4)
  rchm = as.raster(chm)

  ri11 = rumple_index(chm)
  ri12 = rumple_index(rchm)

  expect_equal(ri11, ri12, tolerance = 0.01 * ri11)

  las = lidR:::dummy_las(6000)
  chm = las %>% grid_canopy(2)
  rchm = as.raster(chm)

  ri21 = rumple_index(chm)
  ri22 = rumple_index(rchm)

  expect_equal(ri21, ri22, tolerance = 0.01 * ri21)

  expect_gt(ri21, ri11)
  expect_gt(ri22, ri12)
})
