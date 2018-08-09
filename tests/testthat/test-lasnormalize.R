context("lasnormalize")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
lidar = readLAS(LASfile)

test_that("Each ground point is at 0 with knnidw", {
  lidr_options(progress = FALSE)
  suppressWarnings(lasnormalize(lidar, method = "knnidw", k = 10L, p = 1))
  Z0 = lidar@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})

test_that("Each ground point is at 0 with delaunay", {
  suppressWarnings(lasnormalize(lidar, method = "delaunay"))
  Z0 = lidar@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})

test_that("Each ground point is at 0 with kriging", {
  suppressWarnings(lasnormalize(lidar, method = "kriging", k = 10L))
  Z0 = lidar@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
})