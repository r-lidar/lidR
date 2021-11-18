context("retrieve")

las = clip_rectangle(megaplot, 684766, 5017870, 684866, 5017970)
las@data = las@data[,c(1:4, 6:7)]
# retrieve_pulses

test_that("retrieve_pulses works", {
  las = retrieve_pulses(las)
  n = names(las)
  expect_true("pulseID" %in% n)
  expect_is(las$pulseID, "integer")
})

test_that("retrieve_pulses fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(retrieve_pulses(las), "No 'gpstime' attribute found")
})

test_that("retrieve_pulses warn if gpstime zeroed", {
  las@data$gpstime <- 0
  expect_warning(retrieve_pulses(las), "populated with 0")

  las <- suppressWarnings(retrieve_pulses(las))
  expect_true(all(las$pulseID == 1L))
})

# retrieve_flightlines

test_that("retrieve_flightlines works", {
  las = retrieve_flightlines(las)

  n = names(las)
  expect_true("flightlineID" %in% n)
  expect_equal(tabulate(las$flightlineID), c(12565,4496))
})

test_that("retrieve_flightlines fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(retrieve_flightlines(las), "No 'gpstime' attribute found")
})

test_that("retrieve_flightlines warn if gpstime zeroed", {
  las@data$gpstime <- 0
  expect_warning(retrieve_flightlines(las), "populated with 0")

  las <- suppressWarnings(retrieve_pulses(las))
  expect_true(all(las$flightlineID == 1L))
})

# retrieve_scanlines

# Connot be tested because no data with a valid scan direction flag
#test_that("retrieve_scanlines works", {
#  las = retrieve_scanlines(las)
#
#  n = names(las)
#  expect_true("retrieve_scanlinesID" %in% n)
#  expect_equal(tabulate(las$flightlineID), c(12565,4496))
#})

test_that("retrieve_scanlines fails if no gpstime", {
  las@data[["gpstime"]] <- NULL
  expect_error(retrieve_scanlines(las), "No 'gpstime' attribute found")
})

