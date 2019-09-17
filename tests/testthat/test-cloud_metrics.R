context("cloud_metrics")

LASfile <- system.file("extdata", "example.laz", package = "rlas")
las     <- readLAS(LASfile)
ctg     <- suppressMessages(readLAScatalog(c(LASfile, LASfile)))
opt_progress(ctg) <- FALSE

test_that("lasmetrics works with LAS", {
  x = cloud_metrics(las, mean(Z))
  expect_equal(x, 975.9, tol = 0.1)

  x = cloud_metrics(las, ~mean(Z))
  expect_equal(x, 975.9, tol = 0.1)

  x = cloud_metrics(las, ~list(mean(Z), max(Z)))
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  expect_equal(x[[1]], 975.9, tol = 0.1)
  expect_equal(x[[2]], 978.3, tol = 0.1)
})

test_that("cloud_metrics works with catalog_apply", {
  x = catalog_apply(ctg, cloud_metrics, func = ~mean(Z))
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  expect_equal(x[[1]], 975.9, tol = 0.1)
  expect_equal(x[[2]], 975.9, tol = 0.1)

  x = catalog_apply(ctg, cloud_metrics, func = ~list(mean(Z), max(Z)))
  expect_is(x, "list")
  expect_equal(length(x), 2L)
  expect_is(x[[1]], "list")
  expect_is(x[[2]], "list")
})

