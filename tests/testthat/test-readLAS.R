context("readLAS")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")

test_that("read las works", {
  las = readLAS(LASfile)
  expect_is(las, "LAS")
})

test_that("read unexisting file fails", {
  expect_error(readLAS("unexistingfile.las"))
})

las = readLAS(LASfile, XYZonly = TRUE)

test_that("read XYZ only works", {
  expect_equal(dim(las@data)[2], 4)
})

test_that("read multiple files works", {
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(3*dim(las@data)[1], dim(las2@data)[1])
})