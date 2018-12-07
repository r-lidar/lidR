context("grid_metrics")

las = lidR:::dummy_las(2000)
las@proj4string = sp::CRS("+init=epsg:4326")

test_that("grid_metrics returns a RasterLayer", {
  x = grid_metrics(las, list(`mean Z` = mean(Z)))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,1))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), "mean.Z")
})

test_that("grid_metrics returns a RasterLayer aligned with the start option", {
  x = grid_metrics(las, length(Z), start = c(10,10))

  expect_true(is(x, "RasterLayer"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(6,6,1))
  expect_equal(raster::extent(x), raster::extent(-10,110,-10,110))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), "V1")
})

test_that("grid_metrics returns a RasterBrick with a LAS", {
  x = grid_metrics(las, list(meanZ = mean(Z), maxZ = max(Z)))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(5,5,2))
  expect_equal(raster::extent(x), raster::extent(0,100,0,100))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("meanZ", "maxZ"))
})

test_that("grid_metrics returns a RasterBrick aligned with the start option", {
  x = grid_metrics(las, list(meanZ = mean(Z), maxZ = max(Z)), start = c(10,10))

  expect_true(is(x, "RasterBrick"))
  expect_equal(raster::res(x), c(20,20))
  expect_equal(dim(x), c(6,6,2))
  expect_equal(raster::extent(x), raster::extent(-10,110,-10,110))
  expect_equal(x@crs, las@proj4string)
  expect_equal(names(x), c("meanZ", "maxZ"))
})

test_that("grid_metrics returns a correct raster layer (tricky case)", {

  las2 = lasfilter(las, X < 20 | X > 70)
  out = grid_metrics(las2, max(Z))

  expect_equal(dim(out), c(5, 5, 1))
  expect_equal(raster::res(out), c(20, 20))

  las2 = lasfilter(las, (X < 20 | X > 70) & (Y < 20 | Y > 70))
  out = grid_metrics(las2, max(Z), 10)

  expect_equal(dim(out), c(10, 10, 1))
  expect_equal(raster::res(out), c(10, 10))
})

test_that("grid_metrics return a correct raster brick (tricky case)", {

  las2 = lasfilter(las, (X < 20 | X > 80) & (Y < 20 | Y > 80))
  out = suppressWarnings(grid_metrics(las2, list(mean(Z), max(Z)), 10))

  expect_true(is(out, "RasterBrick"))
  expect_equal(dim(out), c(10, 10, 2))
  expect_equal(raster::res(out), c(10, 10))
})


file <- system.file("extdata", "Megaplot.laz", package="lidR")
ctg = catalog(file)
las = readLAS(file, select = "xyz", filter = "-keep_first")
opt_cores(ctg) <- 1
opt_chunk_size(ctg) <- 160
ctg@chunk_options$alignment = c(684750, 5017760)
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg) <- FALSE
opt_select(ctg) = "xyz"
opt_filter(ctg) = "-keep_first"

test_that("grid_metric return the same both with LAScatalog and LAS", {
  m1 = grid_metrics(ctg, length(Z), 20)
  m2 = grid_metrics(las, length(Z), 20)
  expect_equal(m1, m2)

  m1 = grid_metrics(ctg, list(length(Z), mean(Z)), 20)
  m2 = grid_metrics(las, list(length(Z), mean(Z)), 20)
  m1@data@isfactor = m2@data@isfactor
  expect_equal(m1, m2)

  expect_error(grid_metrics(ctg, mean(Intensity), 20), "Intensity")
  expect_error(grid_metrics(las, mean(Intensity), 20), "Intensity")
})

test_that("grid_metric return the same both with catalog and las + grid alignment", {
  m1 = grid_metrics(ctg, length(Z), 20, start = c(10,10))
  m2 = grid_metrics(las, length(Z), 20, start = c(10,10))
  expect_equal(m1, m2)
})

test_that("grid_metric works with a raster as input", {
  r = raster::raster(round(extent(las)-80))
  raster::res(r) = 15
  r[] = runif(ncell(r))

  m1 = grid_metrics(ctg, length(Z), r)
  m2 = grid_metrics(las, length(Z), r)
  expect_equal(m1, m2)

  expect_error(grid_metrics(ctg, mean(Intensity), 20), "Intensity")
  expect_error(grid_metrics(las, mean(Intensity), 20), "Intensity")
})


las = readLAS(file)

test_that("predefined metric set work both with a LAS and LAScatalog", {
  las@data[, ScanAngle := runif(.N)]

  expect_error(grid_metrics(las, .stdmetrics_z), NA)
  expect_error(grid_metrics(las, .stdmetrics_i), NA)
  expect_error(grid_metrics(las, .stdmetrics_rn), NA)
  expect_error(grid_metrics(las, .stdmetrics_ctrl), NA)

  expect_error(grid_metrics(ctg, .stdmetrics_z), NA)
})



