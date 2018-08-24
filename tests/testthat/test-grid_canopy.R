context("grid_canopy")

las = lidR:::dummy_las(10000)

test_that("grid_canopy p2r works", {
  x = grid_canopy(las, "p2r", 4)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(4,4))
  expect_equal(dim(x), c(25,25,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
})

test_that("grid_canopy p2r works 2", {

  dt = data.table::data.table(X = seq(0,10, 0.5), Y = seq(0,10, 0.5), Z = runif(21))
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, "p2r", 2)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-2,12,-2,12))

  x = grid_canopy(las, "p2r", 1)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-1,11,-1,11))
})

test_that("grid_canopy p2r works with subcircle", {

  dt = data.table::data.table(X = 0, Y = 0, Z = 0)
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, "p2r", 0.5, 10)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,10.5,-10.5,10.5))

  dt = data.table::data.table(X = c(0,10), Y = c(0,20), Z = c(0, 5))
  las = suppressWarnings(LAS(dt))

  x = grid_canopy(las, "p2r", 0.5, 10)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::extent(x), raster::extent(-10.5,20.5,-10.5,30.5))
})

LASfile = system.file("extdata", "MixedConifer.laz", package="lidR")
las = readLAS(LASfile, select = "xyzr")
ctg = catalog(LASfile)
cores(ctg) <- 1
tiling_size(ctg) <- 160
buffer(ctg) <- 5
progress(ctg) <- FALSE

test_that("grid_canopy tin works both with LAS and LAScatalog", {
  x = grid_canopy(las, "tin", res = 1)
  y = grid_canopy(ctg, "tin", res = 1)

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(90,90,1))
  expect_equal(raster::extent(x), raster::extent(481260,481350,3812921,3813011))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.0002)
})

test_that("grid_canopy pit-free works both with LAS and LAScatalog", {
  x = grid_canopy(las, "pitfree", res = 1, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))
  y = grid_canopy(ctg, "pitfree", res = 1, thresholds = c(0,2,5,10,15), max_edge = c(0, 1.5))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(1,1))
  expect_equal(dim(x), c(90,90,1))
  expect_equal(raster::extent(x), raster::extent(481260,481350,3812921,3813011))
  expect_equal(x@crs, las@crs)
  expect_equal(names(x), "Z")
  expect_equal(x, y, tolerance = 0.0002)
})

