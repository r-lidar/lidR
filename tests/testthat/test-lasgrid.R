context("lasgrid")

las = lidR:::dummy_las(5000, seeds = 10)

test_that("grid in grid_metric is appropriated", {
  x = grid_metrics(las, length(Z), 20)

  expect_equal(raster::extent(x), raster::extent(0,100,0,100))

  x = grid_metrics(las, length(Z), 10)

  expect_equal(raster::extent(x), raster::extent(0,100,0,100))

  x = grid_metrics(las, length(Z), 15.5)

  expect_equal(raster::extent(x), raster::extent(0, 108.5,0,108.5))
})

test_that("grid in grid_metric is appropriated even with start", {
  x = grid_metrics(las, length(Z), 10, start = c(-5,-5))

  expect_equal(raster::extent(x), raster::extent(-5,105,-5,105))

  x = grid_metrics(las, length(Z), 20, start = c(-5,8))

  expect_equal(raster::extent(x), raster::extent(-5,115,-12,108))
})
