context("lasdecimate")

las = lidR:::dummy_las(100)

test_that("lasdecimate returns an error if pulseID not found", {
  expect_error(lasdecimate(las, density = 0.5, res = 5))
})

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, pulseID = T)

test_that("lasdecimate homogenize works", {
  lasdec = lasdecimate(las, density = 0.5, res = 5)
  xdec = grid_density(lasdec, res = 5)

  expect_true(between(median(xdec$density), 0.5-sd(xdec$density), 0.5+sd(xdec$density) ))

  lasdec = lasdecimate(las, density = 0.8, res = 4)
  xdec = grid_density(lasdec, res = 5)

  expect_true(between(median(xdec$density), 0.8-sd(xdec$density), 0.8+sd(xdec$density) ))
})
