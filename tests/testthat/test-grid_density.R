context("grid_density")

file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)
cores(ctg) <- 1
tiling_size(ctg) <- 160
buffer(ctg) <- 0
progress(ctg) <- FALSE

test_that("grid_density returns the same both with LAScatalog and LAS", {
  d1 = grid_density(ctg)
  d2 = grid_density(las)
  expect_equal(d1, d2)
})
