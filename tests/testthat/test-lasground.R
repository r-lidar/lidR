context("lasground")

file <- system.file("extdata", "Topography.laz", package="lidR")
las = readLAS(file, select = "xyzrn")

test_that("lasground update las object", {
  ws = seq(3,21, 5)
  th = seq(0.1, 2, length.out = length(ws))

  lasground(las, "pmf", ws, th)

  n = names(las@data)

  expect_true("Classification" %in% n)
  expect_equal(unique(las@data$Classification), c(0, 2))
})
