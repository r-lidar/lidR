context("lasidentify")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, "trn", filter = "-keep_xy 684766 5017870 684866 5017970")

# laspulse

test_that("laspulse works", {
  las = laspulse(las)
  n = names(las@data)
  expect_true("pulseID" %in% n)
  expect_is(las$pulseID, "integer")
})

test_that("laspulse fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(laspulse(las), "No 'gpstime' attribute found")
})

test_that("laspulse warn if gpstime zeroed", {
  las@data$gpstime <- 0
  expect_warning(laspulse(las), "populated with 0")

  las <- suppressWarnings(laspulse(las))
  expect_true(all(las$pulseID == 1L))
})

# lasflightline

test_that("lasflightline works", {
  las = lasflightline(las)

  n = names(las@data)
  expect_true("flightlineID" %in% n)
  expect_equal(tabulate(las$flightlineID), c(12565,4496))
})

test_that("lasflightline fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(lasflightline(las), "No 'gpstime' attribute found")
})

test_that("lasflightline warn if gpstime zeroed", {
  las@data$gpstime <- 0
  expect_warning(lasflightline(las), "populated with 0")

  las <- suppressWarnings(laspulse(las))
  expect_true(all(las$flightlineID == 1L))
})

# lasscanline

# Connot be tested because no data with a valid scan direction flag
#test_that("lasscanline works", {
#  las = lasscanline(las)
#
#  n = names(las@data)
#  expect_true("lasscanlineID" %in% n)
#  expect_equal(tabulate(las$flightlineID), c(12565,4496))
#})

test_that("lasscanline fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(lasscanline(las), "No 'gpstime' attribute found")
})
