context("lasrangecorrection")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile)
sensor <- sensor_tracking(las, pmin = 15)

test_that("lasrangecorrection works", {

  expect_error(lasrangecorrection(las, sensor, Rs = 2000), NA)

  las <- lasrangecorrection(las, sensor, Rs = 2000)
  expect_true("RawIntensity" %in% names(las@data))
  expect_true(is.integer(las$Intensity))
  expect_equal(mean(las$Intensity), 1183, tolerance = 0.5)
})

test_that("lasrangecorrection fails with invalid LAS data", {
  las@data[["Intensity"]] <- NULL
  expect_error(lasrangecorrection(las, sensor, Rs = 2000), "No 'Intensity' attribute found")

  las@data[["gpstime"]] <- NULL
  expect_error(lasrangecorrection(las, sensor, Rs = 2000), "No 'gpstime' attribute found in las")
})

test_that("lasrangecorrection fails with invalid sensor data", {

  raster::projection(sensor) <- raster::projection(las)
  tsensor <- sp::spTransform(sensor, sp::CRS("+init=epsg:2008"))
  expect_error(lasrangecorrection(las, tsensor, Rs = 2000), "Point-cloud and sensor positions do not overlap.")

  sensor@data[["gpstime"]] <- NULL
  expect_error(lasrangecorrection(las, sensor, Rs = 2000), "No 'gpstime' attribute found in sensor")

  sensor@data[["gpstime"]] <- 1:8
  expect_error(lasrangecorrection(las, sensor, Rs = 2000), "gpstime range from the sensor does not contain gpstime range from the point-cloud")
})

test_that("lasrangecorrection fails with invalid attribute modification", {

  expect_error(lasrangecorrection(las, sensor, Rs = 2000, elevation = "K"), "No 'K' attribute found in sensor")
  expect_error(lasrangecorrection(las, sensor, Rs = 2000, gpstime = "t"), "No 't' attribute found in sensor")
  expect_error(lasrangecorrection(las, sensor, Rs = 2000, elevation = NULL), "There are only two dimensions in the coordinates of the sensor")
})

test_that("lasrangecorrection works with 3 coordinates SpatialPointsDataFrame", {

  sensor3 <- sensor
  sensor3@coords <- cbind(sensor@coords, sensor$Z)

  las1 <- lasrangecorrection(las, sensor3, Rs = 2000)
  las2 <- lasrangecorrection(las, sensor, Rs = 2000)

  expect_equal(las1, las2)
})

test_that("lasrangecorrection is able to detect unrealistic range corrections", {

  sensor$Z[5] <- 8000
  expect_error(capture.output(lasrangecorrection(las, sensor, Rs = 2000), type = "message"), "Unrealistic range")
})

test_that("lasrangecorrection clamps values to 65535 with warning", {

  las$Intensity[1000] <- 60000L
  expect_warning(lasrangecorrection(las, sensor, Rs = 2000), "1 points have a normalized intensity greater than 65535")

  las1 <- suppressWarnings(lasrangecorrection(las, sensor, Rs = 2000))
  expect_equal(max(las1$Intensity), 65535)
})
