context("lasnormalize")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)
opt_cores(ctg) <- 1
opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(332550, 5238450)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

test_that("Each ground point is at 0 with knnidw", {
  las <- lasnormalize(las, knnidw(k = 10L, p = 1))
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("Each ground point is at 0 with delaunay", {
  las <- lasnormalize(las, tin())
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("Each ground point is at 0 with kriging", {
  las <- lasnormalize(las, kriging(k = 10L))
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  lasunnormalize(las)
})

test_that("lasnormalize work with a catalog", {
  expect_error(lasnormalize(ctg, tin()), "buffer")

  opt_chunk_buffer(ctg) <- 30

  expect_error(lasnormalize(ctg, tin()), "output file")

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = lasnormalize(ctg, tin())
  las2 = readLAS(ctg2)

  expect_equal(nrow(las2@data), nrow(las@data))
})
