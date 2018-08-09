context("grid_canopy")

las = lidR:::dummy_las(10000)

test_that("grid_canopy works", {
  x = grid_canopy(las, 4)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(4,4))
  expect_equal(dim(x), c(25,25,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
})

test_that("grid_canopy works 2", {

  dt = data.table::data.table(X = seq(0,10, 0.5), Y = seq(0,10, 0.5), Z = runif(21))
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, 2)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-2,12,-2,12))

  x = grid_canopy(las, 1)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-1,11,-1,11))
})

test_that("grid_canopy works with subcircle", {

  dt = data.table::data.table(X = 0, Y = 0, Z = 0)
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, 0.5, 10)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,10.5,-10.5,10.5))

  dt = data.table::data.table(X = c(0,10), Y = c(0,20), Z = c(0, 5))
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, 0.5, 10)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,20.5,-10.5,30.5))
})

