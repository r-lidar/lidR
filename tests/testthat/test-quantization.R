context("quantization")

x = runif(100, 1000, 2000)
y = data.table::copy(x)
scale = 0.001
offset = 500

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, select = "xyz")

test_that("Quantization works", {

  lidR:::fast_quantization(x, scale, offset)

  expect_equal(x, round(y, 3), tolerance = 1e-10)
  expect_equal(lidR:::fast_countunquantized(x, scale, offset), 0L)
  expect_equal(lidR:::fast_countunquantized(y, scale, offset), 100L)
})

test_that("Quantization works on las data", {

  xscale = las@header@PHB$`X scale factor`
  xoffset = las@header@PHB$`X offset`
  yscale = las@header@PHB$`Y scale factor`
  yoffset = las@header@PHB$`Y offset`
  zscale = las@header@PHB$`Z scale factor`
  zoffset = las@header@PHB$`Z offset`

  expect_equal(lidR:::fast_countunquantized(las$X, xscale, xoffset), 0L)
  expect_equal(lidR:::fast_countunquantized(las$Y, yscale, yoffset), 0L)
  expect_equal(lidR:::fast_countunquantized(las$Z, zscale, zoffset), 0L)

  x = las@data[["X"]][1]
  las@data[["X"]][1] = x + 0.0001

  expect_equal(lidR:::fast_countunquantized(las$X, xscale, xoffset), 1L)

  lidR:::fast_quantization(las@data[["X"]], xscale, xoffset)

  expect_equal(las@data[["X"]][1], x,  tolerance = 0)
})
