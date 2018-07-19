context("lasfilterdecimate")

las = lidR:::dummy_las(100)

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile)

test_that("lasdecimate homogenize works", {
  lasdec = lasfilterdecimate(las, density = 0.5, res = 5)
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec$point_density), 0.5-sd(xdec$point_density), 0.5+sd(xdec$point_density) ))

  lasdec = lasfilterdecimate(las, density = 0.8, res = 4)
  xdec = grid_density(lasdec, res = 5)

  expect_true(data.table::between(median(xdec$point_density), 0.8-sd(xdec$point_density), 0.8+sd(xdec$point_density) ))
})
