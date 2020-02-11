context("lasnormalize")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)
dtm = grid_terrain(las, 1, tin())

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

test_that("Each ground point is at 0 a RasterLayer", {
  las <- lasnormalize(las, dtm)
  Z0 = las@data[Classification == 2]$Z
  expect_equal(mean(abs(Z0)), 0.06, tol = 0.001)
})

test_that("Absolute elevation is extrabytes(ed)", {

  las <- lasnormalize(las, dtm)

  expect_true(is.null(las@header@VLR$Extra_Bytes))

  las <- lasnormalize(las, dtm, add_lasattribute = TRUE)

  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$data_type, 6L)
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$scale, las@header@PHB$`Z scale factor`)
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$offset, las@header@PHB$`Z offset`)
})

test_that("Error if NAs in DTM", {
  dtm = grid_terrain(las, 2, tin())
  dtm[1500] <- NA

  expect_error(lasnormalize(las, dtm), "not normalizable")
  expect_message(lasnormalize(las, dtm, na.rm = TRUE), "6 points were not normalizable and removed")

  las2 = suppressMessages(lasnormalize(las, dtm, na.rm = TRUE))

  expect_equal(npoints(las2), npoints(las) - 6 )
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

test_that("lasnormalize works with minus symbol", {
  dtm = grid_terrain(las, 2, tin())
  expect_error(las-dtm, NA)
  expect_error(las-tin(), NA)
})

