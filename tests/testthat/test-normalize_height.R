context("normalize_height")

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(LASfile)
ctg = catalog(LASfile)
dtm = grid_terrain(las, 1, tin())

opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(332550, 5238450)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE

test_that("Each ground point is at 0 with knnidw", {
  las <- normalize_height(las, knnidw(k = 10L, p = 1))
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  unnormalize_height(las)
})

test_that("Each ground point is at 0 with delaunay", {
  las <- normalize_height(las, tin())
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))

  unnormalize_height(las)
})

test_that("Each ground point is at 0 with kriging", {
  las <- normalize_height(las, kriging(k = 10L))
  Z0 = las@data[Classification == 2]$Z
  expect_true(all(Z0 == 0))
  unnormalize_height(las)
})

test_that("Each ground point is at 0 a RasterLayer", {
  las <- normalize_height(las, dtm)
  Z0 = las@data[Classification == 2]$Z
  expect_equal(mean(abs(Z0)), 0.06, tol = 0.001)
})

test_that("Absolute elevation is extrabytes(ed)", {

  las <- normalize_height(las, dtm)

  expect_true(is.null(las@header@VLR$Extra_Bytes))

  las <- normalize_height(las, dtm, add_lasattribute = TRUE)

  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$data_type, 6L)
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$scale, las@header@PHB$`Z scale factor`)
  expect_equal(las@header@VLR$Extra_Bytes$`Extra Bytes Description`$Zref$offset, las@header@PHB$`Z offset`)
})

test_that("Error if NAs in DTM", {
  dtm = grid_terrain(las, 2, tin())
  dtm[1500] <- NA

  expect_error(normalize_height(las, dtm), "not normalizable")
  expect_message(normalize_height(las, dtm, na.rm = TRUE), "6 points were not normalizable and removed")

  las2 = suppressMessages(normalize_height(las, dtm, na.rm = TRUE))

  expect_equal(npoints(las2), npoints(las) - 6 )
})

test_that("normalize_height work with a catalog", {
  expect_error(normalize_height(ctg, tin()), "buffer")

  opt_chunk_buffer(ctg) <- 30

  expect_error(normalize_height(ctg, tin()), "output file")

  opt_output_files(ctg) <- paste0(tmpDir(), "file_{XLEFT}_{YBOTTOM}")

  ctg2 = normalize_height(ctg, tin())
  las2 = readLAS(ctg2)

  expect_equal(nrow(las2@data), nrow(las@data))
})

test_that("normalize_height works with minus symbol", {
  dtm = grid_terrain(las, 2, tin())
  expect_error(las-dtm, NA)
  expect_error(las-tin(), NA)
})
