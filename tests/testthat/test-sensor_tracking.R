context("sensor_tracking")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
ctg <- readLAScatalog(LASfile)
opt_chunk_size(ctg) <- 250
opt_chunk_alignment(ctg) <- c(273300,5274300)
opt_chunk_buffer(ctg) <- 40
opt_progress(ctg) <- FALSE

test_that("sensor_tracking works", {

  flightlines <- sensor_tracking(las, pmin = 10)

  expect_is(flightlines, "SpatialPointsDataFrame")
  expect_equal(dim(flightlines), c(8,4))
  expect_equal(names(flightlines), c("Z", "gpstime", "PointSourceID", "npulses"))
  expect_equal(mean(flightlines$Z), 3100, tol = 3)
})

test_that("sensor_tracking returns an empty SpatialPointsDataFrame", {

  flightlines <- sensor_tracking(las, pmin = 50)

  expect_is(flightlines, "SpatialPointsDataFrame")
  expect_equal(dim(flightlines), c(0,4))
  expect_equal(names(flightlines), c("Z", "gpstime", "PointSourceID", "npulses"))
})

test_that("sensor_tracking works with a LAScatalog", {

  flightlines <- sensor_tracking(ctg, pmin = 10)

  expect_is(flightlines, "SpatialPointsDataFrame")
  expect_equal(dim(flightlines), c(8,4))
  expect_equal(names(flightlines), c("Z", "gpstime", "PointSourceID", "npulses"))
  expect_equal(mean(flightlines$Z), 3100, tol = 3)
})

test_that("sensor_tracking hangle errors", {

  las2 = las
  las2@data = las@data[, !"PointSourceID"]
  expect_error(sensor_tracking(las2, pmin = 15), "No 'PointSourceID' attribute found")

  las2@data$PointSourceID <- 0L
  expect_error(sensor_tracking(las2, pmin = 15), "'PointSourceID' attribute is not populated")

  las2$gpstime <- 0
  expect_error(sensor_tracking(las2, pmin = 15), "'gpstime' attribute is not populated")

  las2 = las
  las2$PointSourceID[15] <- 5L
  expect_warning(sensor_tracking(las2, pmin = 15, thin_pulse_with_time = 0), "points with same gpstime")
})
