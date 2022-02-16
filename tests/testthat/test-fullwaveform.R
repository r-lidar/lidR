test_that("interpret waveforn works", {
  skip_on_cran()

  LASfile <- system.file("extdata", "fwf.laz", package="rlas")
  fwf <- readLAS(LASfile)
  expect_error(las <- interpret_waveform(fwf), NA)
  expect_equal(npoints(las), 455168)
})
