context("normalize_intensity")

las <- topography
sensor <- track_sensor(las, Roussel2020(pmin = 15))

test_that("normalize_intensity works", {

  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), NA)

  las <- normalize_intensity(las, range_correction(sensor, Rs = 2000))
  expect_true("RawIntensity" %in% names(las@data))
  expect_true(is.integer(las$Intensity))
  expect_equal(mean(las$Intensity), 1183, tolerance = 0.5)
})

test_that("normalize_intensity fails with invalid LAS data", {
  las@data[["Intensity"]] <- NULL
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "No 'Intensity' attribute found")

  las@data[["gpstime"]] <- NULL
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "No 'gpstime' attribute found in las")
})

test_that("normalize_intensity fails with invalid sensor data", {

  sink(tempfile())

  raster::crs(sensor) <- crs(las)
  tsensor <- sp::spTransform(sensor, sp::CRS("+init=epsg:26917"))
  expect_error(normalize_intensity(las, range_correction(tsensor, Rs = 2000)), "Unrealistic range")

  sensor@data[["gpstime"]] <- NULL
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "No 'gpstime' attribute found in sensor")

  sensor@data[["gpstime"]] <- 1:8
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "gpstime range from the sensor does not contain gpstime range from the point-cloud")

  sink(NULL)
})

test_that("normalize_intensity fails with invalid attribute modification", {

  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000, elevation = "K")), "No 'K' attribute found in sensor")
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000, gpstime = "t")), "No 't' attribute found in sensor")
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000, elevation = NULL)), "There are only two dimensions in the coordinates of the sensor")
})

test_that("normalize_intensity works with 3 coordinates SpatialPointsDataFrame", {

  sensor3 <- sensor
  sensor3@coords <- cbind(sensor@coords, sensor$Z)

  las1 <- normalize_intensity(las, range_correction(sensor3, Rs = 2000))
  las2 <- normalize_intensity(las, range_correction(sensor, Rs = 2000))

  expect_equal(las1, las2)
})

test_that("normalize_intensity is able to detect unrealistic range corrections", {

  sink(tempfile())
  sensor$Z[5] <- 8000
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "Unrealistic range")
  sink(NULL)
})

test_that("normalize_intensity clamps values to 65535 with warning", {

  las$Intensity[1000] <- 60000L
  expect_warning(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "1 points have a normalized intensity greater than 65535")

  las1 <- suppressWarnings(normalize_intensity(las, range_correction(sensor, Rs = 2000)))
  expect_equal(max(las1$Intensity), 65535)
})

