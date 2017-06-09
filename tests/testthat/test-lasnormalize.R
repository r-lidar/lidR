context("lasnormalize")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
lidar = readLAS(LASfile)

test_that("Each ground point is at 0 with knnidw", {
  lnorm = suppressWarnings(lasnormalize(lidar, method = "knnidw", k = 10L))
  Z0 = lnorm@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})

test_that("Each ground point is at 0 with delaunay", {
  lnorm = suppressWarnings(lasnormalize(lidar, method = "delaunay"))
  Z0 = lnorm@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})

test_that("Each ground point is at 0 with kriging", {
  lnorm = suppressWarnings(lasnormalize(lidar, method = "kriging", k = 10L))
  Z0 = lnorm@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})

test_that("lasnormalize support lasmetrics and raster objects", {
  dtm = grid_terrain(lidar, method = "kriging", k = 10L)

  lnorm = suppressWarnings(lasnormalize(lidar, dtm))
  Z0 = lnorm@data[Classification == 2]$Z
  expect_equal(mean(Z0), 0, tolerance = 0.01)

  dtm = as.raster(dtm)

  lnorm = suppressWarnings(lasnormalize(lidar, dtm))
  Z0 = lnorm@data[Classification == 2]$Z
  expect_equal(mean(Z0), 0, tolerance = 0.01)
})
