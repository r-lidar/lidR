context("print")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)

LASfile <- system.file("extdata", "extra_byte.laz", package = "rlas")
las2     <- readLAS(LASfile)

test_that("print works with LAS", {
  sink(tempfile())
  expect_error(print(las), NA)
  sink(NULL)
})

test_that("summary works with LAS", {
  sink(tempfile())
  expect_error(summary(las), NA)
  sink(NULL)
})

test_that("print works with LAS and extra bytes", {
  sink(tempfile())
  expect_error(print(las2), NA)
  sink(NULL)
})

test_that("summary works with LAS and extra bytes", {
  sink(tempfile())
  expect_error(summary(las2), NA)
  sink(NULL)
})
