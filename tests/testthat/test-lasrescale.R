context("lasrescale")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile, "xyz")

test_that("lasrescale works", {
  las2 = suppressMessages(lasrescale(las, xscale = 1, yscale = 1, zscale = 1))
  expect_equal(las2@data[,1:2], round(las@data[,1:2]))
})

test_that("lasroffset works", {
  las2 = suppressMessages(lasreoffset(las, xoffset = 100000, yoffset = 6000000, zoffset = 2))
  expect_equal(las2@data, las@data)
})
