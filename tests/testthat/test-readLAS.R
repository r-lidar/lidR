context("readLAS")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las <- readLAS(LASfile, select = "xyzrn")

test_that("readLAS works", {
  expect_is(las, "LAS")
})

test_that("readLAS returns a error for non-existing file", {
  expect_error(readLAS("unexistingfile.las"), regexp = "File does not exist")
})

test_that("read only XYZ attributes works", {
  expect_equal(names(las@data), c("X", "Y", "Z", "ReturnNumber", "NumberOfReturns"))
  expect_equal(ncol(las@data), 5)
})

test_that("read multiple files works", {
  las2 = readLAS(rep(LASfile, 3))
  expect_equal(3*dim(las@data)[1], dim(las2@data)[1])
})

test_that("readLAS throw warning for invalid files", {
  las@header@PHB$`X scale factor` <- 0.01234
  las@header@PHB$`Y scale factor` <- 0.01234
  las@data$ReturnNumber[1] <- 2L
  f <- tempfile(fileext = ".las")
  rlas:::C_writer(f, as.list(las@header), las@data)
  las  <- suppressWarnings(readLAS(f))

  expect_warning(readLAS(f), "X scale factors")
  expect_warning(readLAS(f), "Y scale factors")
  expect_warning(readLAS(f), "return number")
  expect_equal(las@header@PHB$`X scale factor`, 0.01234)
  expect_equal(las@header@PHB$`Y scale factor`, 0.01234)
})
