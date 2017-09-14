context("grid_catalog")

catalog_options(multicore = 1, tiling_size = 160, buffer = 0)

file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)

test_that("grid_canopy return the same both with catalog and las", {
  chm1 = grid_canopy(ctg)
  chm2 = grid_canopy(las)
  data.table::setorder(chm1, X, Y )
  data.table::setorder(chm2, X, Y )
  expect_equal(chm1, chm2)
})

test_that("grid_density return the same both with catalog and las", {
  d1 = grid_density(ctg)
  d2 = grid_density(las)
  data.table::setorder(d1, X, Y )
  data.table::setorder(d2, X, Y )
  expect_equal(d1, d2)
})

test_that("grid_metric return the same both with catalog and las", {
  m1 = grid_metrics(ctg, .stdmetrics, 20)
  m2 = grid_metrics(las, .stdmetrics, 20)
  data.table::setorder(m1, X, Y )
  data.table::setorder(m2, X, Y )
  expect_equal(m1, m2)
})