context("grid_density")

file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg  <- catalog(file)
las  <- readLAS(file)

opt_chunk_buffer(ctg)    <- 0
opt_chunk_size(ctg)      <- 160
opt_chunk_alignment(ctg) <- c(0, 92)
opt_progress(ctg)        <- FALSE

test_that("grid_density returns the same both with LAScatalog and LAS", {
  d1 <- grid_density(ctg)
  d2 <- grid_density(las)
  expect_equal(d1, d2)
})
