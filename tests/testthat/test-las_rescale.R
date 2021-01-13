context("las_rescale")

las <- example

test_that("las_rescale works", {
  las2 = suppressMessages(las_rescale(las, xscale = 1, yscale = 1, zscale = 1))
  expect_equal(las2@data[,1:2], round(las@data[,1:2]))
})

test_that("lasroffset works", {
  las2 = suppressMessages(las_reoffset(las, xoffset = 100000, yoffset = 6000000, zoffset = 2))
  expect_equal(las2@data, las@data)
})
