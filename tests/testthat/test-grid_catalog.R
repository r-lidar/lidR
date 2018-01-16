context("grid_catalog")

catalog_options(multicore = 1, tiling_size = 160, buffer = 0)

file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)

test_that("grid_canopy returns the same both with catalog and las", {
  chm1 = grid_canopy(ctg)
  chm2 = grid_canopy(las)
  data.table::setorder(chm1, X, Y )
  data.table::setorder(chm2, X, Y )
  expect_equal(chm1, chm2)
})

test_that("grid_density returns the same both with catalog and las", {
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


file <- system.file("extdata", "Topography.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file)

test_that("grid_terrain returns the same both with catalog and las", {
  catalog_options(multicore = 1, tiling_size = 180, buffer = 30)
  t1 = grid_terrain(ctg, 2, "knnidw", k = 5)
  t2 = grid_terrain(las, 2, "knnidw", k = 5)
  data.table::setorder(t1, X, Y )
  data.table::setorder(t2, X, Y )
  expect_equal(t1, t2, check.attributes = F)

  catalog_options(multicore = 1, tiling_size = 180, buffer = 30)
  t1 = suppressMessages(grid_terrain(ctg, 2, "delaunay"))
  t2 = suppressMessages(grid_terrain(las, 2, "delaunay"))
  data.table::setkey(t1, X, Y)
  data.table::setkey(t2, X, Y)
  diff = t1[t2]
  diffZ = abs(diff$Z - diff$i.Z)
  expect_lt(mean(diffZ, na.rm = TRUE), 0.01)
})
