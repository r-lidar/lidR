context("readLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")

test_that("readLAS works", {
  las = readLAS(LASfile)
  expect_is(las, "LAS")
})

test_that("readLAS returns a error for non-existing file", {
  expect_error(readLAS("unexistingfile.las"), regexp = "File does not exist")
})

test_that("read only XYZ attributes works", {
  las = readLAS(LASfile, select = "xyz")
  expect_equal(names(las@data), c("X", "Y", "Z"))
  expect_equal(ncol(las@data), 3)
})

test_that("read multiple files works", {
  las  = readLAS(LASfile)
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(3*dim(las@data)[1], dim(las2@data)[1])
})
