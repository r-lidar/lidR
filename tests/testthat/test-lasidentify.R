context("lasidentify")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, "trn", filter = "-keep_xy 684766 5017870 684866 5017970")

test_that("laspulse works", {
  las = laspulse(las)

  n = names(las@data)
  expect_true("pulseID" %in% n)
})

test_that("laspulse warn if gpstime zeroed", {
  las@data$gpstime <- 0
  expect_warning(laspulse(las), "populated with 0")
})

test_that("lasflightlines works", {
  las = lasflightline(las)

  n = names(las@data)
  expect_true("flightlineID" %in% n)
  expect_equal(tabulate(las$flightlineID), c(12565,4496))
})
