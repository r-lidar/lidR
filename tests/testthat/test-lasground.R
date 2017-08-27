context("lasground")

file <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(file, select = "xyz")

test_that("lasground update las object", {
  lasground(las)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(0, 2))
})
