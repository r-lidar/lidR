context("grid_density")

ctg  <- megaplot_ctg
las  <- megaplot

opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 160
opt_chunk_alignment(ctg) <- c(0, 92)
opt_progress(ctg)        <- FALSE

test_that("grid_density returns the same both with LAScatalog and LAS", {
  d1 <- grid_density(ctg)
  d2 <- grid_density(las)
  expect_equal(d1, d2)
})


test_that("grid_density returns pulse density", {
  las <- retrieve_pulses(las)
  d1 <- grid_density(las)

  expect_is(d1, "RasterBrick")
  expect_equal(names(d1), c("point_density", "pulse_density"))
})

