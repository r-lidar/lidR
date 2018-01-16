context("readLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")

test_that("read las works", {
  las = readLAS(LASfile)
  expect_is(las, "LAS")
})

test_that("read unexisting file fails", {
  expect_error(readLAS("unexistingfile.las"))
})

test_that("read XYZ only works", {
  las = readLAS(LASfile, select = "xyz")
  expect_equal(ncol(las@data), 3)
})

test_that("read multiple files works", {
  las  = readLAS(LASfile)
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(3*dim(las@data)[1], dim(las2@data)[1])
})

test_that("print las works", {
  las  = readLAS(LASfile)
  sink(tempfile())
  expect_error(summary(las), NA)
  sink(NULL)
})