context("decimate_points")

las = megaplot
ctg = megaplot_ctg

test_that("decimate_points homogenize algorithm works", {
  lasdec = decimate_points(las, homogenize(0.5,5))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("decimate_points random algorithm works", {
  lasdec = decimate_points(las, random(0.5))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.5-sd(xdec[], na.rm = TRUE), 0.5+sd(xdec[], na.rm = TRUE) ))
})

test_that("decimate_points highest algorithm works", {
  lasdec = decimate_points(las, highest(2))
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec[], na.rm = TRUE), 0.23, 0.25))
})

test_that("decimate_points fails with unproper catalog options", {

  expect_error(decimate_points(ctg, homogenize(0.8,5)), "output file")
})

test_that("decimate_points works with a LAScatalog", {

  ctg <- random_2files_250points
  opt_output_files(ctg) <- "{tempdir()}/{ID}"
  ctg2 <- decimate_points(ctg, highest(10))

  expect_is(ctg2, "LAScatalog")
  expect_gt(npoints(ctg), npoints(ctg2))
})
