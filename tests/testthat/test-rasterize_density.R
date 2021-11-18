context("rasterize_density")

ctg  <- megaplot_ctg
las  <- megaplot

opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 160
opt_chunk_alignment(ctg) <- c(50, 92)
opt_progress(ctg)        <- FALSE

slr = lidR:::raster_class()
mlr = lidR:::raster_multilayer_class()

test_that("rasterize_density returns the same both with LAScatalog and LAS", {

  d1 <- rasterize_density(ctg)
  d2 <- rasterize_density(las)

  expect_is(d1, slr)
  expect_equal(mean(lidR:::raster_values(d1)), 1.49, tolerance = 0.001)
  expect_equal(lidR:::raster_names(d2), "density")
  expect_equivalent(d1, d2)
})

test_that("rasterize_density returns pulse density", {

  las <- retrieve_pulses(las)
  d1  <- rasterize_density(las)

  expect_is(d1, mlr)
  expect_equal(lidR:::raster_names(d1), c("point_density", "pulse_density"))
})

test_that("grid_density is backward compatible", {
  las <- retrieve_pulses(las)
  d1 <- grid_density(las)

  expect_is(d1, "RasterBrick")
  expect_equal(lidR:::raster_names(d1), c("point_density", "pulse_density"))
})

