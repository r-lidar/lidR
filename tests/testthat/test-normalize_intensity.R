context("normalize_intensity")

las <- topography
sensor <- track_sensor(las, Roussel2020(pmin = 15))

test_that("normalize_intensity works", {

  expect_error(las <- normalize_intensity(las, range_correction(sensor, Rs = 2000)), NA)
  expect_true("RawIntensity" %in% names(las))
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

  p = sf::st_sfc(sf::st_point(c(0, -10000, 0)))
  geom = sf::st_geometry(sensor) + p
  sf::st_geometry(sensor) = geom
  sf::st_crs(sensor) <- st_crs(las)

  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "Unrealistic range")

  sensor[["gpstime"]] <- NULL
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "No 'gpstime' attribute found in sensor")

  sensor[["gpstime"]] <- 1:8
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "gpstime range from the sensor does not contain gpstime range from the point-cloud")

  sink(NULL)
})


test_that("normalize_intensity works with 2 coordinates points", {

  sensor2 <- sf::st_zm(sensor)
  sensor2$Z <- sf::st_coordinates(sensor)[,3]

  las1 <- normalize_intensity(las, range_correction(sensor2, Rs = 2000))
  las2 <- normalize_intensity(las, range_correction(sensor, Rs = 2000))

  expect_equal(las1, las2)

  expect_error(normalize_intensity(las, range_correction(sensor2, Rs = 2000, elevation = "K")), "No 'K' attribute found in sensor")
  expect_error(normalize_intensity(las, range_correction(sensor2, Rs = 2000, gpstime = "t")), "No 't' attribute found in sensor")
  expect_error(normalize_intensity(las, range_correction(sensor2, Rs = 2000, elevation = NULL)), "There are only two dimensions in the coordinates of the sensor")
})


test_that("normalize_intensity is able to detect unrealistic range corrections", {

  sink(tempfile())
  sensor$geometry[[5]][2] <- 8000
  expect_error(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "Unrealistic range")
  sink(NULL)
})

test_that("normalize_intensity clamps values to 65535 with warning", {

  las$Intensity[1000] <- 60000L
  expect_warning(normalize_intensity(las, range_correction(sensor, Rs = 2000)), "1 points have a normalized intensity greater than 65535")

  las1 <- suppressWarnings(normalize_intensity(las, range_correction(sensor, Rs = 2000)))
  expect_equal(max(las1$Intensity), 65535)
})

