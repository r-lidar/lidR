context("lasrangecorrection")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)

test_that("lasrangecorrection works", {
  sensor <- sensor_tracking(las, pmin = 15)
  expect_error(lasrangecorrection(las, sensor, Rs = 2000), NA)
})
