context("lasfilterdecimate")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)

test_that("lasfilterdecimate homogenize works", {
  lasdec = lasfilterdecimate(las, homogenize(0.5,5))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))

  lasdec = lasfilterdecimate(las, homogenize(0.8,4))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.8-sd(xdec[], na.rm = TRUE), 0.8+sd(xdec[], na.rm = TRUE) ))
})

test_that("lasfilterdecimate fails with unproper catalog options", {

  expect_error(lasfilterdecimate(ctg, homogenize(0.8,5)), "output file")

})
