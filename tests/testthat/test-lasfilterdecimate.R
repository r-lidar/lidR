context("lasfilterdecimate")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)

test_that("lasfilterdecimate homogenize algorithm works", {
  lasdec = lasfilterdecimate(las, homogenize(0.5,5))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("lasfilterdecimate random algorithm works", {
  lasdec = lasfilterdecimate(las, random(0.5))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("lasfilterdecimate highest algorithm works", {
  lasdec = lasfilterdecimate(las, highest(2))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.23, 0.25))
})

test_that("lasfilterdecimate fails with unproper catalog options", {

  expect_error(lasfilterdecimate(ctg, homogenize(0.8,5)), "output file")
})

test_that("lasfilterdecimate works with a LAScatalog", {

  ctg <- lidR:::catalog_generator(200)
  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  ctg2 <- lasfilterdecimate(ctg, highest(10))

  expect_is(ctg2, "LAScatalog")
  expect_gt(npoints(ctg), npoints(ctg2))
})
