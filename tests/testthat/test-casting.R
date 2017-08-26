context("casting")

library(raster)
LASfile <- system.file("extdata", "Megaplot.laz", package = "lidR")
las = readLAS(LASfile)
lasflightline(las)

test_that("as.raster return a correct raster layer (simple case)", {

  out = grid_metrics(las, max(Z))
  out = as.raster(out)

  expect_true(is(out, "RasterLayer"))
  expect_equal(dim(out), c(13, 12, 1))
  expect_equal(res(out), c(20, 20))

  out = grid_metrics(las, max(Z), 10)
  out = as.raster(out)

  expect_true(is(out, "RasterLayer"))
  expect_equal(dim(out), c(24, 24, 1))
  expect_equal(res(out), c(10, 10))
})

test_that("as.raster return a correct raster stack (simple case)", {

  out = grid_metrics(las, .stdmetrics_z)
  depth = ncol(out) - 2
  out = as.raster(out)

  expect_true(is(out, "RasterStack"))
  expect_equal(dim(out), c(13, 12, depth))
  expect_equal(res(out), c(20, 20))

  out = grid_metrics(las, .stdmetrics_z, 10)
  depth = ncol(out) - 2
  out = as.raster(out)

  expect_true(is(out, "RasterStack"))
  expect_equal(dim(out), c(24, 24, depth))
  expect_equal(res(out), c(10, 10))
})


test_that("as.raster return a correct raster layer (tricky case)", {

  out = grid_metrics(las, max(Z))
  out = out[ X < 684800 | X > 684900]
  as.lasmetrics(out, 20)
  out = as.raster(out)

  expect_true(is(out, "RasterLayer"))
  expect_equal(dim(out), c(13, 12, 1))
  expect_equal(res(out), c(20, 20))

  out = grid_metrics(las, max(Z), 10)
  out = out[(X < 684830 | X > 684900) & (Y < 5017900 | Y > 5017940)]
  as.lasmetrics(out, 10)
  out = as.raster(out)

  expect_true(is(out, "RasterLayer"))
  expect_equal(dim(out), c(24, 24, 1))
  expect_equal(res(out), c(10, 10))
})

test_that("as.raster return a correct raster stack (tricky case)", {

  out = grid_metrics(las, .stdmetrics_z, 10)
  depth = ncol(out) - 2
  out = out[(X < 684830 | X > 684900) & (Y < 5017900 | Y > 5017940)]
  as.lasmetrics(out, 10)
  out = as.raster(out)

  expect_true(is(out, "RasterStack"))
  expect_equal(dim(out), c(24, 24, depth))
  expect_equal(res(out), c(10, 10))
})

test_that("as.raster return a correct raster stack (simple case)", {

  out = grid_metrics(las, .stdmetrics_z)
  depth = ncol(out) - 2
  out = as.raster(out)

  expect_true(is(out, "RasterStack"))
  expect_equal(dim(out), c(13, 12, depth))
  expect_equal(res(out), c(20, 20))

  out = grid_metrics(las, .stdmetrics_z, 10)
  depth = ncol(out) - 2
  out = as.raster(out)

  expect_true(is(out, "RasterStack"))
  expect_equal(dim(out), c(24, 24, depth))
  expect_equal(res(out), c(10, 10))
})

test_that("as.raster return a correct raster layer with duplicated entries", {

  out = grid_metrics(las, max(Z), splitlines = TRUE)
  out = as.raster(out)

  expect_true(is(out, "RasterLayer"))
  expect_equal(dim(out), c(13, 12, 1))
  expect_equal(res(out), c(20, 20))
})

test_that("as.raster return the same output both with dcast and sp", {

  out = grid_metrics(las, max(Z))
  out1 = as.raster(out)
  out2 = as.raster(out, spbackend = TRUE)
  expect_equal(out1, out2)
})

test_that("as.raster guess the resolution properly", {

  out = grid_metrics(las, max(Z))
  out = out[ X < 684800 | X > 684900]
  out = as.raster(out)

  expect_equal(res(out), c(20, 20))

  out = grid_metrics(las, max(Z), 10)
  out = out[(X < 684830 | X > 684900) & (Y < 5017900 | Y > 5017940)]
  out = as.raster(out)

  expect_equal(res(out), c(10, 10))
})
