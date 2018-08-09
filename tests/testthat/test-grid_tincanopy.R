context("grid_tincanopy")

LASfile = system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyzr")
ctg = catalog(LASfile)
cores(ctg) <- 1
tiling_size(ctg) <- 160
buffer(ctg) <- 5
progress(ctg) <- FALSE

test_that("grid_tincanopy ", {
  x = grid_tincanopy(las, res = 1, thresholds = 0, max_edge = 0)
  y = grid_tincanopy(ctg, res = 1, thresholds = 0, max_edge = 0)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(90,90,1))
  expect_equal(raster::extent(x), raster::extent(481260,481350,3812921,3813011))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.0002)
})

test_that("grid_metrics returns a RasterLayer", {
  x = grid_tincanopy(las, res = 1, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
  y = grid_tincanopy(ctg, res = 1, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(90,90,1))
  expect_equal(raster::extent(x), raster::extent(481260,481350,3812921,3813011))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.0002)
})

