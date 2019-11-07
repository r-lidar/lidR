context("stdmetric")

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, filter = "-keep_xy 684766.4 5017773 684866.4 5017973")

xall = grid_metrics(las, .stdmetrics, 20)

test_that("stdmetric return the same result than .stdmetric", {
  y = grid_metrics(las, stdmetrics(X,Y,Z, Intensity, ReturnNumber, Classification), 20)
  expect_identical(xall, y)
})

test_that("stdmetric_i return the same result than .stdmetric_i", {
  x = grid_metrics(las, .stdmetrics_i, 20)
  y = grid_metrics(las, stdmetrics_i(Intensity, Z, Classification), 20)

  expect_identical(x,y)
})

test_that("stdmetric_i return the same result than stdmetric", {
  y = grid_metrics(las, .stdmetrics_i, 20)
  cols = names(y)
  x = raster::subset(xall,cols)

  x@data@isfactor <- y@data@isfactor <- FALSE

  expect_identical(x,y)
})

test_that("stdmetric_z return the same result than stdmetric", {
  y = grid_metrics(las, .stdmetrics_z, 20)
  cols = names(y)
  x = raster::subset(xall,cols)

  x@data@isfactor <- y@data@isfactor <- FALSE

  expect_identical(x,y)
})

test_that("stdmetric_rn return the same result than stdmetric", {
  y = grid_metrics(las, .stdmetrics_rn, 20)
  cols = names(y)
  x = raster::subset(xall,cols)

  x@data@isfactor <- y@data@isfactor <- FALSE

  expect_identical(x,y)
})


test_that("stdmetric_pulse works", {
  las = laspulse(las)
  expect_error(grid_metrics(las, .stdmetrics_pulse, 20), NA)
})

