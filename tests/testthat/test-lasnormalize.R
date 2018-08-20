context("lasnormalize")

lidr_options(progress = FALSE)

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)
cores(ctg) <- 1
tiling_size(ctg) <- 160
ctg@clustering_options$alignment = c(332550, 5238450)
buffer(ctg) <- 0
progress(ctg) <- FALSE

test_that("Each ground point is at 0 with knnidw", {
  lasnormalize(las, method = "knnidw", k = 10L, p = 1)
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("Each ground point is at 0 with delaunay", {
  lasnormalize(las, method = "tin")
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("Each ground point is at 0 with kriging", {
  lasnormalize(las, method = "kriging", k = 10L)
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("lasnormalize work with a catalog", {
  expect_error(lasnormalize(ctg, method = "tin"), "buffer")

  buffer(ctg) <- 30

  expect_error(lasnormalize(ctg, method = "tin"), "output file")

  output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = lasnormalize(ctg, method = "tin")
  las2 = readLAS(ctg2)

  expect_equal(nrow(las2@data), nrow(las@data))
})
